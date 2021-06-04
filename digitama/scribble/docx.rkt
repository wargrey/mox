#lang racket/base

(provide (all-defined-out))

(require scribble/core)

(require racket/class)
(require racket/list)
(require racket/string)
(require racket/format)

(require file/convertible)

(require digimon/archive)
(require digimon/dtrace)

(require "docx/metainfo.rkt")
(require "docx/app.rkt")
(require "docx/style.rkt")
(require "docx/misc.rkt")

(require "docx/story/document.rkt")

(require "shared/render.rkt")
(require "shared/scribble.rkt")

(require "package/content.type.rkt")
(require "package/relationship.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-docx-link-sections (make-parameter #f))

(define (render-mixin %)
  (class % (super-new)
    (inherit-field style-file style-extra-files)
    (inherit render-part render-flow render-block)
    (inherit format-number number-depth install-file)
    (inherit extract-part-style-files link-render-style-at-element)

    (define/override (current-render-mode) (list docx-render-mode))
    (define/override (get-suffix) docx-suffix)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (collect-part-tags p ci number)
      (for ([t (part-tags p)])
        (let ([t (generate-tag t ci)])
          (collect-put! ci t
                        (vector (or (part-title-content p) '("???"))
                                (add-current-tag-prefix t)
                                number
                                docx-part-tag)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (render-one p ri ?dest)
      (define title/raw (part-title-content p))
      (define plain-title (and title/raw (content->string title/raw)))
      (define-values (clean-properties doc-id doc-version doc-date) (mox-sift-property 'wargrey (style-properties (part-style p))))

      (start-render)
      
      (let ([docblocks (render-part p ri)]
            [main-part  (mox-story-part doc-id 'document.xml)]
            [style-part (mox-story-part doc-id 'styles.xml)]
            [font-part (mox-story-part doc-id 'fontTable.xml)]
            [theme-part (mox-story-part doc-id 'theme.xml)]
            [footnote-part (mox-story-part doc-id 'footnotes.xml)]
            [endnote-part (mox-story-part doc-id 'endnotes.xml)]
            [settings-part (mox-story-part doc-id 'settings.xml)]
            [websettings-part (mox-story-part doc-id 'webSettings.xml)]
            [docProps (opc-word-properties-markup-entries "/~a" plain-title (reverse (unbox &authors)) doc-version doc-date clean-properties)])
        (zip-create #:strategy 'fixed
                    (current-output-port)
                    (list (opc-content-types-markup-entry
                           (append (list main-part style-part font-part theme-part footnote-part endnote-part settings-part websettings-part)
                                   (for/list ([type.entry (in-list docProps)])
                                     (cons (string-append "/" (archive-entry-name (cdr type.entry)))
                                           (car type.entry)))))
                          (opc-relationships-markup-entry
                           "/" ; package relationship
                           (append (for/list ([type.prop (in-list docProps)])
                                     (opc-make-internal-relationship (mox-relation-id (car type.prop))
                                                                     (archive-entry-name (cdr type.prop))
                                                                     (car type.prop)))
                                   (list (opc-make-internal-relationship main-part))))
                          (map cdr docProps)
                          (opc-relationships-markup-entry
                           (car main-part) ; document.xml relationship
                           (map opc-make-internal-relationship
                                (list style-part theme-part font-part
                                      footnote-part endnote-part
                                      settings-part websettings-part)))
                          (opc-word-document-markup-entry (car main-part) docblocks)
                          (opc-word-style-markup-entry (car style-part))
                          (opc-word-theme-markup-entry (car theme-part))
                          (opc-word-font-markup-entry (car font-part))
                          (opc-word-footnote-markup-entry (car footnote-part))
                          (opc-word-endnote-markup-entry (car endnote-part))
                          (opc-word-settings-markup-entry (car settings-part))
                          (opc-word-websettings-markup-entry (car websettings-part))))))

    (define/override (render-part-content p ri)
      (define c (part-title-content p))
      (define depth (number-depth (collected-info-number (part-collected-info p ri))))
      (define-values (sn sp) (scribble-style->values (part-style p)))
      
      (dtrace-debug #:topic docx-render-mode
                    "§[~a]: ~a" depth (content->string c))
      
      (cons (word-section (if (not c) null (render-content c p ri)) sn sp
                          (current-tag-prefixes) (link-render-style-mode (current-link-render-style))
                          depth)

            (apply append
                   (render-flow (part-blocks p) p ri #f)
                   (map (lambda (s) (render-part s ri))
                        (part-parts p)))))

    (define/override (render-paragraph p part ri)
      (define c (paragraph-content p))
      (define s (paragraph-style p))
      (define-values (sn sp) (scribble-style->values s))

      (when (and (eq? sn 'author) c)
        (set-box! &authors
                  (cons (content->string c)
                        (unbox &authors))))
      
      (list (word-paragraph (if (not c) null (render-content c part ri)) sn sp)))
    
    (define/override (render-table i part ht inline?)
      (define flowss (table-blockss i))

      (define tick? (member (style-name (table-style i))
                            (list 'boxed "defmodule" "RktBlk")))

      (cond
        [(null? flowss) null]

        [(and tick? (not (in-code?)))
          (displayln "```racket")
          (parameterize ([in-code? #t])
            (render-table i part ht inline?))
          (displayln "```")]

        [else
          (define strs (map (lambda (flows)
                              (map (lambda (d)
                                     (if (eq? d 'cont)
                                         d
                                         (let ([o (open-output-string)])
                                           (parameterize ([current-output-port o])
                                             (render-block d part ht #f))
                                           (regexp-split
                                            #rx"\n"
                                            (regexp-replace #rx"\n$"
                                                            (get-output-string o)
                                                            "")))))
                                   flows))
                            flowss))
          (define widths (map (lambda (col)
                                (for/fold ([d 0]) ([i (in-list col)])
                                  (if (eq? i 'cont)
                                      0
                                      (apply max d (map string-length i)))))
                              (apply map list strs)))
          (define x-length (lambda (col) (if (eq? col 'cont) 0 (length col))))
          (for/fold ([indent? #f]) ([row (in-list strs)])
            (let ([h (apply max 0 (map x-length row))])
              (let ([row* (for/list ([i (in-range h)])
                            (for/list ([col (in-list row)])
                              (if (i . < . (x-length col))
                                  (list-ref col i)
                                  "")))])
                (for/fold ([indent? indent?]) ([sub-row (in-list row*)])
                  
                  (for/fold ([space? #f])
                      ([col (in-list sub-row)]
                       [w (in-list widths)])
                    (let ([col (if (eq? col 'cont) "" col)])
                      (display (regexp-replace* #rx"\uA0" col " "))
                      (display (make-string (max 0 (- w (string-length col))) #\space)))
                    #t)
                  (newline)
                  #t)))
            #t)])
      null)

    (define/override (render-itemization i part ht)
      (let ([flows (itemization-blockss i)])
        (if (null? flows)
            null
            (append*
             (begin (printf "* ")
                    (render-flow (car flows) part ht #t))
             (for/list ([d (in-list (cdr flows))])
               (printf "* ")
               (render-flow d part ht #f))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (render-content c part ri)
      (cond [(string? c) (render-text c part ri)]
            [(list? c) (apply append (for/list ([c (in-list c)]) (render-content c part ri)))]
            [(target-element? c)
             (let-values ([(sn sp) (scribble-style->values (element-style c))]
                          [(cs tag) (values (element-content c) (target-element-tag c))])
               (list (word-bookmark (gensym (car tag))
                                    (render-content cs part ri)
                                    (tag-key tag ri) sn sp)))]
            [(link-element? c)
             (let-values ([(sn sp) (scribble-style->values (element-style c))]
                          [(cs tag) (values (element-content c) (link-element-tag c))])
               (list (word-hyperlink (gensym (car tag))
                                     (render-content (cond [(not (null? cs)) cs]
                                                           [else (let ([v (resolve-get part ri tag)])
                                                                   (if (vector? v)
                                                                       (strip-aux (or (vector-ref v 0) "???"))
                                                                       (list "[missing]")))])
                                                     part ri)
                                     (tag-key tag ri) sn sp)))]
            [(element? c)
             (let-values ([(sn sp) (scribble-style->values (element-style c))])
               (dtrace-debug "~a" (word-run-texts (render-content (element-content c) part ri) sn sp))
               (when (render-element? c) ((render-element-render c) this part ri))
               (list (word-run-texts (render-content (element-content c) part ri) sn sp)))]
            [(multiarg-element? c)
             (let-values ([(sn sp) (scribble-style->values (multiarg-element-style c))])
               (list (render-content (multiarg-element-contents c) part ri)))]
            [(delayed-element? c) (render-content (delayed-element-content c ri) part ri)]
            [(traverse-element? c) (render-content (traverse-element-content c ri) part ri)]
            [(part-relative-element? c) (render-content (part-relative-element-content c ri) part ri)]
            [(convertible? c) (render-text (convert c 'text) part ri)]
            [else (render-text c part ri)]))
    
    (define/private (content-style e)
      (cond
       [(element? e) (element-style e)]
       [(multiarg-element? e) (multiarg-element-style e)]
       [else #f]))

    (define in-bold? (make-parameter #f))
    (define in-italic? (make-parameter #f))
    (define in-code? (make-parameter #f))
    (define in-link? (make-parameter #f))
    (define preserving-spaces? (make-parameter #f))

    (define (bold? i)
      (and (element? i) (eq? (element-style i) 'bold)))

    (define (italic? i)
      (and (element? i) (eq? (element-style i) 'italic)))

    (define (emph? i)
      (and (element? i) (eq? (element-style i) 'emph)))

    (define (code? i)
      (and (element? i)
           (let ([s (element-style i)])
             (or (eq? 'tt s)
                 (and (style? s)
                      (style-name s)
                      (regexp-match? #rx"^Rkt[A-Z]" (style-name s)))))))

    (define (link? i)
      (let ([s (content-style i)])
        (and (style? s) (findf target-url? (style-properties s)))))

    (define (link-from i)
      (target-url-addr (findf target-url? (style-properties (content-style i)))))

    (define (preserve-spaces? i)
      (and (element? i)
           (let ([s (element-style i)])
             (or (eq? 'hspace s)
                 (and (style? s)
                      (eq? 'hspace (style-name s)))))))

    (define (sanitize-parens str)
      (regexp-replace #rx"[\\(\\)]" str "\\&"))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties s))
        (define note? (equal? (style-name s) "refcontent"))
        (define toc? (equal? (style-name s) 'table-of-contents))

        (super render-nested-flow i part ri starting-item?)))

    (define/override (table-of-contents part ri)
      (define t (super table-of-contents part ri))
      (cond
        [(current-docx-link-sections)
         ;; Table generated by `table-of-contents` always has one
         ;; column, and each row has one paragraph that starts
         ;; with a 'hspace element to indent
         (nested-flow
          (style 'table-of-contents null)
          (for/list ([p (map car (table-blockss t))])
            (define c (paragraph-content p))
            (define keep-c (cdr c))
            (define (spaces->depth n)
              (add1 (quotient (- n 4) 2)))
            (for/fold ([p (paragraph plain keep-c)]) ([s (in-range
                                                          (spaces->depth
                                                           (string-length (car (element-content (car c))))))])
              (nested-flow (style "refcontent" null) (list p)))))]
        [else t]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define &authors (box null))

    (define (start-render)
      (set-box! &authors null))
    
    (define (render-text t part ri)
      (list (cond [(string? t) t]
                  [else (~s t)])))))
