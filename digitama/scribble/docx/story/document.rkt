#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/string)

(require digimon/archive)

(require sgml/xexpr)

(require "../../shared/typed/scribble.rkt")

(require "../../package/partname.rkt")
(require "../../package/standards.rkt")
(require "../../package/xmlns.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Word-Run-Content (U Word-Run Word-Annotation String))

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
   [style-name : Style-Name]
   [style-properties : Style-Properties])
  #:type-name Word-Run-Texts
  #:transparent)

(struct word-hyperlink Word-Run
  ([field-id : Symbol]
   [content : (Listof Word-Run-Content)]
   [tag : (List Symbol String)]
   [style-name : Style-Name]
   [style-properties : Style-Properties])
  #:type-name Word-Hyperlink
  #:transparent)

(struct word-paragraph Word-Block
  ([content : (Listof Word-Run-Content)]
   [style-name : Style-Name]
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

(define word-run->xexpr : (-> (U Word-Run String) (Listof Xexpr))
  (lambda [r]
    (cond [(string? r)
           (list (word-contents->run r))]
          [(word-run-text? r)
           (list (word-contents->run (word-run-text-content r) (word-run-text-style-name r) (word-run-text-style-properties r)))]
          [(word-run-texts? r)
           (list (word-contents->run (word-run-texts-contents r) (word-run-texts-style-name r) (word-run-texts-style-properties r)))]
          [(word-hyperlink? r) (word-hyperlink->field r)]
          [else (list (word-run/unrecognized r))])))

(define word-annotation->xexpr : (-> Word-Annotation (Listof Xexpr))
  (lambda [a]
    (cond [(word-bookmark? a)
           (word-cross-structure-annotation (word-bookmark-id a) (word-bookmark-key (cadr (word-bookmark-tag a)))
                                            'w:bookmarkStart 'w:bookmarkEnd
                                            (word-bookmark-content a))]
          [else (list (word-run/unrecognized a))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-paragraph->attlist : (-> Word-Paragraph Xexpr-AttList)
  (lambda [p]
    null))

(define word-paragraph->runs : (-> Word-Paragraph (Listof Xexpr))
  (lambda [p]
    (word-content->runs (word-paragraph-content p))))

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
    
    (list 'w:hyperlink `([w:anchor . ,anchor]
                         [w:history . "true"]
                         [w:tooltip . ,(format "[~a] ~a" (car tag) anchor)])
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
                 (for/list : (Listof (Listof Xexpr)) ([rc (if (list? t) (in-list t) (in-value t))])
                   (cond [(string? rc) (list (word-text rc #:w:tag w:t))]
                         [(Word-Annotation? rc) (word-annotation->xexpr rc)]
                         [else (word-run->xexpr rc)]))))))

(define word-style-xexpr : (-> String Xexpr)
  (lambda [style]
    `(w:pPr () ((w:pStyle ([w:val . ,style]))))))

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
(define word-cross-structure-annotation : (-> Symbol String Symbol Symbol (Listof Word-Run-Content) (Listof Xexpr))
  (lambda [id name tag-start tag-end cs]
    (define id-v (symbol->immutable-string id))
    
    (append (list `(,tag-start ([w:id . ,id-v] [w:name . ,name])))
            (word-content->runs cs)
            (list `(,tag-end ([w:id . ,id-v]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
