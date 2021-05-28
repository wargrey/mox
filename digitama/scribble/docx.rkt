#lang racket/base

(provide (all-defined-out))

(require scribble/core)

(require racket/class)
(require racket/list)
(require racket/string)

(require digimon/archive)
(require digimon/dtrace)

(require "docx/metainfo.rkt")
(require "docx/app.rkt")
(require "docx/style.rkt")
(require "docx/misc.rkt")

(require "docx/story/document.rkt")

(require "shared/render.rkt")

(require "package/content.type.rkt")
(require "package/relationship.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-docx-link-sections (make-parameter #f))

(define (render-mixin %)
  (class %
    (inherit-field style-file style-extra-files)
    (inherit render-part render-flow render-block format-number number-depth install-file)
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
      (define document (render-part p ri))
      
      (let ([main-part  (mox-story-part doc-id 'document.xml)]
            [style-part (mox-story-part doc-id 'styles.xml)]
            [font-part (mox-story-part doc-id 'fontTable.xml)]
            [theme-part (mox-story-part doc-id 'theme.xml)]
            [footnote-part (mox-story-part doc-id 'footnotes.xml)]
            [endnote-part (mox-story-part doc-id 'endnotes.xml)]
            [settings-part (mox-story-part doc-id 'settings.xml)]
            [websettings-part (mox-story-part doc-id 'webSettings.xml)]
            [docProps (opc-word-properties-markup-entries "/~a" plain-title (list "wargrey" "gyoudmon") doc-version doc-date clean-properties)])
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
                                (list style-part font-part theme-part
                                      footnote-part endnote-part
                                      settings-part websettings-part)))
                          (opc-word-document-markup-entry (car main-part))
                          (opc-word-style-markup-entry (car style-part))
                          (opc-word-theme-markup-entry (car theme-part))
                          (opc-word-font-markup-entry (car font-part))
                          (opc-word-footnote-markup-entry (car footnote-part))
                          (opc-word-endnote-markup-entry (car endnote-part))
                          (opc-word-settings-markup-entry (car settings-part))
                          (opc-word-websettings-markup-entry (car websettings-part))))))

    (define/override (render-part-content p ri)
      (cons (word-section (and (part-title-content p) (render-content (part-title-content p) p ri))
                          (style-name (part-style p)) (style-properties (part-style p))
                          (current-tag-prefixes)
                          (link-render-style-mode (current-link-render-style))
                          (number-depth (collected-info-number (part-collected-info p ri))))

            (append (render-flow (part-blocks p) p ri #f)
                    (map (lambda (s) (render-part s ri))
                         (part-parts p)))))

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

    (define/override (render-paragraph p part ri)
      (cond
        [else
         (define o (open-output-string))
         (parameterize ([current-output-port o])
           (super render-paragraph p part ri))
         ;; 1. Remove newlines so we can re-wrap the text.
         ;;
         ;; 2. Combine adjacent code spans into one. These result from
         ;; something like @racket[(x y)] being treated as multiple
         ;; RktXXX items rather than one. (Although it would be
         ;; more-correct to handle them at that level, I don't easily see
         ;; how. As a result I'm handling it after-the-fact, at the
         ;; text/Markdown stage.)
         (define to-wrap (regexp-replaces (get-output-string o)
                                          '([#rx"\n" " "]   ;1
                                            [#rx"``" ""]))) ;2
         (define lines (list (string-trim to-wrap)))
         (write-string (car lines))])
      (newline)
      null)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

    (define/override (render-content i part ri)
      (define (recurse-wrapped str param)
        (display str)
        (begin0
          (parameterize ([param #t])
            (render-content i part ri))
          (display str)))

      (cond
        [(and (code? i) (not (in-code?)))
          (recurse-wrapped "`" in-code?)]

        [(and (bold? i) (not (in-bold?)) (not (in-code?)))
          (recurse-wrapped "**" in-bold?)]

        [(and (italic? i) (not (in-italic?)) (not (in-code?)))
          (recurse-wrapped "_" in-italic?)]

        [(and (emph? i) (not (in-code?)))
         (display "​_") ;; zero-width space, underscore
         (begin0
             (super render-content i part ri)
           (display "_​"))] ;; underscore, zero-width space

        [(and (preserve-spaces? i) (not (preserving-spaces?)))
          (parameterize ([preserving-spaces? #t])
            (render-content i part ri))]

        [(and (link? i) (not (in-link?)))
          (let ([link (link-from i)])
            (display "[")
            (begin0
              (parameterize ([in-link? #t])
                (render-content i part ri))
              (printf "](~a)" (sanitize-parens link))))]

        [(and (link-element? i)
              (current-docx-link-sections)
              (not (in-link?))
              ;; Link to a part within this document?
              (let ([vec (resolve-get part ri (link-element-tag i))])
                (and (vector? vec)
                     (= 4 (vector-length vec))
                     (eq? docx-part-tag (vector-ref vec 3))
                     vec)))
         => (lambda (vec)
              (define s (string-append
                         (let ([s (if (vector-ref vec 2)
                                      (format-number (vector-ref vec 2) '() #t)
                                      '())])
                           (if (null? s)
                               ""
                               (string-append (car s) " ")))
                         (content->string (vector-ref vec 0))))
              (display "[")
              (begin0
                (parameterize ([in-link? #t])
                  (super render-content i part ri))
                (display "](#")
                (display (regexp-replace* #" "
                                          (regexp-replace* #rx"[^a-zA-Z0-9_ -]" (string-downcase s) "")
                                          #"-"))
                (display ")")))]

        [else (super render-content i part ri)]))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties s))
        (define note? (equal? (style-name s) "refcontent"))
        (define toc? (equal? (style-name s) 'table-of-contents))

        (super render-nested-flow i part ri starting-item?)))

    (define/override (render-other i part ht)
      (cond
        [(symbol? i)
         (display (case i
                    [(mdash) "\U2014"]
                    [(ndash) "\U2013"]
                    [(ldquo) "\U201C"]
                    [(rdquo) "\U201D"]
                    [(lsquo) "\U2018"]
                    [(rsquo) "\U2019"]
                    [(lang) "<"]
                    [(rang) ">"]
                    [(rarr) "->"]
                    [(nbsp) "\uA0"]
                    [(prime) "'"]
                    [(alpha) "\u03B1"]
                    [(infin) "\u221E"]
                    [else (error 'markdown-render "unknown element symbol: ~e"
                                 i)]))]
        [(string? i)
         (let* ([i (cond
                     [(in-code?)
                      (regexp-replace** i '([#rx"``" . "\U201C"]
                                            [#rx"''" . "\U201D"]))]
                     [(or (in-link?)
                          (regexp-match? #rx"^[(]" i)
                          (regexp-match? #rx"[]][(]" i))
                      (regexp-replace* #px"([#_*`\\[\\(\\]\\)]{1})" i "\\\\\\1")]
                     [else
                      ;; Avoid escaping parentheses
                      (regexp-replace* #px"([#_*`\\[\\]]{1})" i "\\\\\\1")])]
                [i (if (preserving-spaces?)
                       (regexp-replace* #rx" " i "\uA0")
                       i)])
           (display i))]
        [else (write i)])
      null)

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

    (super-new)))

(define (regexp-replace** str ptns&reps)
  (for/fold ([str str])
            ([ptn (map car ptns&reps)]
             [rep (map cdr ptns&reps)])
    (regexp-replace* ptn str rep)))
