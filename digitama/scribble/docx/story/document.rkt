#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/string)

(require digimon/archive)
(require digimon/format)
(require digimon/dtrace)

(require sgml/xexpr)

(require "../../shared/typed/scribble.rkt")

(require "../../package/partname.rkt")
(require "../../package/standards.rkt")
(require "../../package/xmlns.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Word-Run-Content (U Word-Run Word-Annotation String))

(struct word-style
  ([name : Style-Name]
   [properties : Style-Properties])
  #:type-name Word-Style
  #:transparent)

(struct Word-Block word-style ())
(struct Word-Run word-style ())
(struct Word-Annotation word-style ())

(struct word-bookmark Word-Annotation
  ([id : Integer]
   [content : (Listof Word-Run-Content)]
   [tag : (List Symbol String)])
  #:type-name Word-Bookmark
  #:transparent)

(struct word-run-text Word-Run
  ([content : String])
  #:type-name Word-Run-Text
  #:transparent)

(struct word-run-texts Word-Run
  ([contents : (Listof Word-Run-Content)])
  #:type-name Word-Run-Texts
  #:transparent)

(struct word-hyperlink Word-Run
  ([content : (Listof Word-Run-Content)]
   [tag : (List Symbol String)])
  #:type-name Word-Hyperlink
  #:transparent)

(struct word-paragraph Word-Block
  ([content : (Listof Word-Run-Content)])
  #:type-name Word-Paragraph
  #:transparent)

(struct word-section word-paragraph
  ([tag-prefixes : (Listof String)]
   [link-render-style : Symbol]
   [numseqs : (Listof String)]
   [depth : Natural])
  #:type-name Word-Section
  #:transparent)

(struct word-list Word-Block
  ([items : (Listof Word-Block)])
  #:type-name Word-List
  #:transparent)

(struct word-nested-flow Word-Block
  ([blocks : (Listof Word-Block)])
  #:type-name Word-Nested-Flow
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-document-markup-entry : (->* (String (Listof Word-Block)) (#:utc Integer) Archive-Entry)
  (lambda [part-name docblocks #:utc [ts #false]]
    (define story : Xexpr
      (list 'w:document

            (append `([xmlns:w . ,(assert (opc-xmlns 'Docx:W))]
                      [xmlns:w10 . ,(assert (opc-xmlns 'Docx:W10))]
                      [xmlns:w14 . ,(assert (opc-xmlns 'Docx:W14))]
                      [xmlns:w15 . ,(assert (opc-xmlns 'Docx:W15))]
                      [xmlns:w16 . ,(assert (opc-xmlns 'Docx:W16))]
                      [xmlns:w16se . ,(assert (opc-xmlns 'Docx:W16se))]
                      [xmlns:w16cid . ,(assert (opc-xmlns 'Docx:W16cid))]
                      [xmlns:w16cex . ,(assert (opc-xmlns 'Docx:W16cex))]
                      [xmlns:w16sdtdh . ,(assert (opc-xmlns 'Docx:W16sdtdh))]
                      [xmlns:wp . ,(assert (opc-xmlns 'Docx:WP))]
                      [xmlns:wp14 . ,(assert (opc-xmlns 'Docx:WP14))]
                      [xmlns:wpc . ,(assert (opc-xmlns 'Docx:WPC))]
                      [xmlns:wpi . ,(assert (opc-xmlns 'Docx:WPI))]
                      [xmlns:wpg . ,(assert (opc-xmlns 'Docx:WPG))]
                      [xmlns:wps . ,(assert (opc-xmlns 'Docx:WPS))]
                      [xmlns:wne . ,(assert (opc-xmlns 'Docx:WNE))])
                    (mox-drawing-xmlns)
                    (mox-compatibility-xmlns))
            
            (list (list 'w:body null
                        (word-blocks->xexprs docblocks)))))
    
    (make-archive-ascii-entry #:utc-time ts #:comment "Primer 2.3, 2006"
                              (xexpr->bytes story #:prolog? #true)
                              (opc-part-name-normalize/zip part-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blocks
(define word-blocks->xexprs : (-> (Listof Word-Block) (Listof Xexpr))
  (lambda [blocks]
    (apply append (map word-block->xexpr blocks))))

(define word-block->xexpr : (->* (Word-Block) (Natural) (Listof Xexpr))
  (lambda [db [indent 0]]
    (cond [(word-section? db) (list (word-section->xexpr db))]
          [(word-paragraph? db) (list (word-paragraph->xexpr (word-paragraph-content db) (word-style-name db) (word-style-properties db)))]
          [(word-list? db)
           (apply append
                  (for/list : (Listof (Listof Xexpr)) ([item (in-list (word-list-items db))])
                    (cond [(word-paragraph? item)
                           (list (word-paragraph->xexpr (word-paragraph-content item) "ListParagraph" (word-style-properties item)
                                                        (list (word-list-paragraph-style-xexpr (word-style-name item) indent))))]
                          [(word-list? item) (word-block->xexpr item (+ indent 1))]
                          [else (word-block->xexpr item indent)])))]
          [(word-nested-flow? db)
           (apply append
                  (for/list : (Listof (Listof Xexpr)) ([block (in-list (word-nested-flow-blocks db))])
                    (cond [(word-paragraph? block)
                           (list (word-nested-paragraph->xexpr (word-paragraph-content block) (word-style-name db) (word-style-properties db)))]
                          [(word-nested-flow? block) (word-block->xexpr block (+ indent 1))]
                          [else (word-block->xexpr block indent)])))]
          [else (list (list 'w:p null (list (word-run/unrecognized db))))])))

(define word-section->xexpr : (-> Word-Section Xexpr)
  (lambda [s]
    (define sname (word-style-name s))
    (define depth (word-section-depth s))
    (define numseqs (word-section-numseqs s))
    (define content (word-paragraph-content s))

    (define p:style
      (cond [(string? sname) sname]
            [(eq? depth 0) "Title"]
            [else (string-append "Heading" (number->string depth))]))

    (word-paragraph->xexpr (cond [(null? numseqs) content]
                                 [else (list* (car numseqs) ". " content)])
                           p:style (word-style-properties s))))

(define word-paragraph->xexpr : (->* ((Listof Word-Run-Content)) (Style-Name Style-Properties (Listof Xexpr)) Xexpr)
  (lambda [pc [style #false] [properties null] [additions null]]
    (define p:style
      (cond [(string? style) style]
            [else "Normal"]))
    
    (list 'w:p null
          (cons (word-paragraph-style-xexpr p:style)
                (append additions (word-content->runs pc))))))

(define word-nested-paragraph->xexpr : (->* ((Listof Word-Run-Content)) (Style-Name Style-Properties Natural) Xexpr)
  (lambda [pc [style #false] [properties null] [indent 0]]
    (list 'w:p null
          (cons (word-nested-paragraph-style-xexpr style indent)
                (word-content->runs pc)))))

(define word-paragraph-style-xexpr : (-> String Xexpr)
  (lambda [style]
    `(w:pPr () ((w:pStyle ([w:val . ,style]))))))

(define word-list-paragraph-style-xexpr : (-> Style-Name Natural Xexpr)
  (lambda [style indent]
    `(w:numPr () ((w:ilvl  ([w:val . ,(number->string indent)]) ())
                  (w:numId ([w:val . ,(if (eq? style 'ordered) "1" "3")]) ())))))

(define word-nested-paragraph-style-xexpr : (-> Style-Name Natural Xexpr)
  (lambda [style indent]
    (define w:wnd : (Listof Xexpr)
      (cond [(<= indent 0) null]
            [else (list (list 'w:wnd `([w:left . ,(number->string (* indent 360))])))]))

    (define w:jc : (Listof Xexpr)
      (case style
        [(center left right both distribute end highKashida lowKashida)
         (list (list 'w:jc `([w:val . ,(symbol->immutable-string style)])))]
        [else null]))
    
    (list 'w:pPr null (append w:wnd w:jc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run and Run Content
(define word-run->xexpr : (-> (U Word-Run String) (Listof Xexpr))
  (lambda [r]
    (cond [(string? r)
           (list (word-contents->run r))]
          [(word-run-text? r)
           (list (word-contents->run (word-run-text-content r) (word-style-name r) (word-style-properties r)))]
          [(word-run-texts? r)
           (list (word-contents->run (word-run-texts-contents r) (word-style-name r) (word-style-properties r)))]
          [(word-hyperlink? r) (word-hyperlink->field r)]
          [else (list (word-run/unrecognized r))])))

(define word-annotation->xexpr : (-> Word-Annotation (Listof Xexpr))
  (lambda [a]
    (cond [(word-bookmark? a)
           (word-cross-structure-annotation (word-bookmark-id a) (word-bookmark-key (cadr (word-bookmark-tag a)))
                                            'w:bookmarkStart 'w:bookmarkEnd
                                            (word-bookmark-content a))]
          [else (list (word-run/unrecognized a))])))

(define word-run-style-xexpr : (-> Style-Name Style-Properties (Option Xexpr))
  (lambda [style properties]
    (cond [(string? style)
           `(w:rPr () ((w:rStyle ([w:val . ,style]))))]
          [(not style) #false]
          [else (case style
                  [(italic)      `(w:rPr () ((w:i)))]
                  [(bold)        `(w:rPr () ((w:b)))]
                  [(subscript)   `(w:rPr () ((w:vertAlign ([w:val . "subscript"]))))]
                  [(superscript) `(w:rPr () ((w:vertAlign ([w:val . "superscript"]))))]
                  [else #false])])))

(define word-hyperlink->field : (-> Word-Hyperlink (Listof Xexpr))
  (lambda [hl]
    (define tag (word-hyperlink-tag hl))
    (define anchor (word-bookmark-key (cadr tag)))
    
    (word-complex-field (string-append "REF " anchor " \\h")
                        (word-hyperlink-content hl))))

(define word-hyperlink->run : (-> Word-Hyperlink Xexpr)
  (lambda [hl]
    (define tag (word-hyperlink-tag hl))
    (define anchor (word-bookmark-key (cadr tag)))
    (define ?tooltip (findf hover-property? (word-style-properties hl)))
    
    (list 'w:hyperlink `([w:anchor . ,anchor]
                         [w:history . "true"]
                         [w:tooltip . ,(if (hover-property? ?tooltip) (hover-property-text ?tooltip) (format "[~a] ~a" (car tag) anchor))])
          (list (list 'w:r null
                      (word-content->runs (word-hyperlink-content hl)))))))

(define word-content->runs : (-> (U Word-Run-Content (Listof Word-Run-Content)) (Listof Xexpr))
  (lambda [cs]
    (apply append
           (for/list : (Listof (Listof Xexpr)) ([c (if (list? cs) (in-list cs) (in-value cs))])
             (if (Word-Annotation? c)
                 (word-annotation->xexpr c)
                 (word-run->xexpr c))))))

(define word-contents->run : (->* ((U Word-Run-Content (Listof Word-Run-Content))) (Style-Name Style-Properties #:w:tag Symbol) Xexpr)
  (lambda [t [style #false] [properties null] #:w:tag [w:t 'w:t]]
    
    (list 'w:r null
          (apply append
                 (let ([pstyle (word-run-style-xexpr style properties)])
                   (cond [(not pstyle) null]
                         [else (list pstyle)]))
                 (for/list : (Listof (Listof Xexpr)) ([rc (if (list? t) (in-list t) (in-value t))])
                   (cond [(Word-Annotation? rc) (word-annotation->xexpr rc)]
                         [(Word-Run? rc) (word-run->xexpr rc)]
                         [(eq? style 'hspace) (list (word-text (~space (string-length rc)) #:w:tag w:t))]
                         [(eq? style 'newline) (list word-br)]
                         [else (list (word-text rc #:w:tag w:t))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fields. Fundamentals and Markup Language Reference 17.16.2
(define word-simple-field : (-> String (U Word-Run-Content (Listof Word-Run-Content)) Xexpr)
  (lambda [instr fldvalues]
    (list 'w:fldSimple `([w:instr . ,instr])
          (word-content->runs fldvalues))))

; TODO: fields can be nested
(define word-complex-field : (-> (U String (Listof String)) (U Word-Run-Content (Listof Word-Run-Content)) (Listof Xexpr))
  (lambda [instrs fldvalues]
    (append (list '(w:r () ((w:fldChar ([w:fldCharType . "begin"])))))
            (for/list : (Listof Xexpr) ([instr (if (list? instrs) (in-list instrs) (in-value instrs))])
              (word-contents->run instr #:w:tag 'w:instrText))
            (list '(w:r () ((w:fldChar ([w:fldCharType . "separate"])))))
            (word-content->runs fldvalues)
            (list '(w:r () ((w:fldChar ([w:fldCharType . "end"]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotations. Primer 2.14
(define word-cross-structure-annotation : (-> Integer String Symbol Symbol (Listof Word-Run-Content) (Listof Xexpr))
  (lambda [id name tag-start tag-end cs]
    (define id-v (number->string id))
    
    (append (list `(,tag-start ([w:id . ,id-v] [w:name . ,name])))
            (word-content->runs cs)
            (list `(,tag-end ([w:id . ,id-v]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-br : Xexpr `(w:br))

(define word-text : (-> String [#:w:tag Symbol] Xexpr)
  (lambda [t #:w:tag [w:t 'w:t]]
    (cond [(string=? t "") `(,w:t () (,t))]
          [(char-blank? (string-ref t 0)) `(,w:t ([xml:space "preserve"]) (,t))]
          [(char-blank? (string-ref t (sub1 (string-length t)))) `(,w:t ([xml:space "preserve"]) (,t))]
          [else `(,w:t () (,t))])))

(define word-run/unrecognized : (-> Any Xexpr)
  (lambda [v]
    (word-contents->run (format "~s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-bookmark-key : (-> String String)
  (lambda [name]
    (string-replace name " " "-")))
