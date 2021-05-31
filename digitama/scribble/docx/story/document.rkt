#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require digimon/archive)

(require sgml/xexpr)

(require "../../shared/typed/scribble.rkt")

(require "../../package/partname.rkt")
(require "../../package/standards.rkt")
(require "../../package/xmlns.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Word-Run-Content (U Word-Run Word-Annotation))

(struct Word-Block ())
(struct Word-Run ())
(struct Word-Annotation ())

(struct word-bookmark Word-Annotation
  ([id : Symbol]
   [content : (Listof Word-Run-Content)]
   [tag : (List Symbol String)]
   [style : Style-Name]
   [properties : Style-Properties])
  #:type-name Word-Bookmark
  #:transparent)

(struct word-run-text Word-Run
  ([content : String]
   [style-name : (U String Symbol False)]
   [style-properties : Style-Properties])
  #:type-name Word-Run-Text
  #:transparent)

(struct word-run-texts Word-Run
  ([contents : (Listof Word-Run-Content)]
   [style-name : (U String Symbol False)]
   [style-properties : Style-Properties])
  #:type-name Word-Run-Texts
  #:transparent)

(struct word-hyperlink Word-Run
  ([content : (Listof Word-Run-Content)]
   [tag : (List Symbol String)]
   [style-name : (U String Symbol False)]
   [style-properties : Style-Properties])
  #:type-name Word-Hyperlink
  #:transparent)

(struct word-paragraph Word-Block
  ([content : (Listof Word-Run-Content)]
   [style-name : (U String Symbol False)]
   [style-properties : Style-Properties])
  #:type-name Word-Paragraph
  #:transparent)

(struct word-section word-paragraph
  ([tag-prefixes : (Listof String)]
   [link-render-style : Symbol]
   [depth : Natural])
  #:type-name Word-Section
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-document-markup-entry : (->* (String (Listof Word-Block)) (#:utc Integer) Archive-Entry)
  (lambda [part-name docblocks #:utc [ts #false]]
    (define body : (Listof Xexpr)
      (for/list : (Listof Xexpr) ([db (in-list docblocks)])
        (cond [(word-section? db) (word-section->xexpr db)]
              [(word-paragraph? db) (word-paragraph->xexpr db)]
              [else (list 'w:p null (list (word-run/unrecognized db)))])))
    
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
            (list (list 'w:body null body))))
    
    (make-archive-ascii-entry #:utc-time ts #:comment "Primer 2.3, 2006"
                              (xexpr->bytes story #:prolog? #true)
                              (opc-part-name-normalize/zip part-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-section->xexpr : (-> Word-Section Xexpr)
  (lambda [s]
    (define attlist (word-paragraph->attlist s))
    (define runs (word-paragraph->runs s))
    (define sname (word-paragraph-style-name s))
    (define depth (word-section-depth s))

    (define p:style
      (cond [(string? sname) sname]
            [(eq? depth 0) "Title"]
            [else (string-append "Heading" (number->string depth))]))

    (list 'w:p attlist
          (cons (word-style-xexpr p:style)
                runs))))

(define word-paragraph->xexpr : (-> Word-Paragraph Xexpr)
  (lambda [p]
    (define attlist (word-paragraph->attlist p))
    (define runs (word-paragraph->runs p))
    (define sname (word-paragraph-style-name p))

    (define p:style
      (cond [(string? sname) sname]
            [else "Normal"]))
    
    (list 'w:p attlist
          (cons (word-style-xexpr p:style)
                runs))))

(define word-run->xexpr : (-> Word-Run Xexpr)
  (lambda [r]
    (cond [(word-run-text? r) (list 'w:r null (list (list 'w:t null (list (word-run-text-content r)))))]
          [(word-run-texts? r) (list 'w:r null (word-contents->runs (word-run-texts-contents r)))]
          [(word-hyperlink? r) (word-hyperlink->run r)]
          [else (word-run/unrecognized r)])))

(define word-annotation->xexpr : (-> Word-Annotation (Listof Xexpr))
  (lambda [a]
    (cond [(word-bookmark? a)
           (word-cross-structure-annotation (word-bookmark-id a) (cadr (word-bookmark-tag a))
                                            'w:bookmarkStart 'w:bookmarkEnd
                                            (word-bookmark-content a))]
          [else (list (word-run/unrecognized a))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-paragraph->attlist : (-> Word-Paragraph Xexpr-AttList)
  (lambda [p]
    null))

(define word-paragraph->runs : (-> Word-Paragraph (Listof Xexpr))
  (lambda [p]
    (word-contents->runs (word-paragraph-content p))))

(define word-hyperlink->run : (-> Word-Hyperlink Xexpr)
  (lambda [hl]
    (define tag (word-hyperlink-tag hl))
    (define anchor (cadr tag))
    
    (list 'w:hyperlink `([w:anchor . ,anchor]
                         [w:history . "true"]
                         [w:tooltip . ,(format "[~a] ~a" (car tag) anchor)])
          (list (list 'w:r null
                      (word-contents->runs (word-hyperlink-content hl)))))))

(define word-contents->runs : (-> (Listof Word-Run-Content) (Listof Xexpr))
  (lambda [cs]
    (apply append
           (for/list : (Listof (Listof Xexpr)) ([c (in-list cs)])
             (if (Word-Annotation? c)
                 (word-annotation->xexpr c)
                 (list (word-run->xexpr c)))))))

(define word-style-xexpr : (-> String Xexpr)
  (lambda [style]
    `(w:pPr () ((w:pStyle ([w:val . ,style]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotations. Primer 2.14
(define word-cross-structure-annotation : (-> Symbol String Symbol Symbol (Listof Word-Run-Content) (Listof Xexpr))
  (lambda [id name tag-start tag-end cs]
    (define id-v (symbol->immutable-string id))
    
    (append (list `(,tag-start ([w:id . ,id-v] [w:name . ,name])))
            (word-contents->runs cs)
            (list `(,tag-end ([w:id . ,id-v]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-run/unrecognized : (-> Any Xexpr)
  (lambda [v]
    (list 'w:r null (list (list 'w:t null (list (format "~s" v)))))))
