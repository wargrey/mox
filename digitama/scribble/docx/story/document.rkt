#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require "../../package/partname.rkt")
(require "../../package/standards.rkt")
(require "../../package/xmlns.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct word-block
  ()
  #:type-name Word-Block
  #:transparent)

(struct word-run
  ([text : String]
   [style-name : (U String Symbol False)]
   [style-properties : (Listof Any)])
  #:type-name Word-Run
  #:transparent)

(struct word-paragraph word-block
  ([content : (Listof (U String Word-Run))]
   [style-name : (U String Symbol False)]
   [style-properties : (Listof Any)])
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
              [else (list 'w:p)])))
    
    (define story : Xexpr
      (list 'w:document (append `([xmlns:w . ,(assert (opc-xmlns 'Docx:W))]
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

    (list 'w:p attlist
          (cons `(w:pPr () ((w:pStyle ([w:val . "Title"]))))
                runs))))

(define word-paragraph->xexpr : (-> Word-Paragraph Xexpr)
  (lambda [p]
    (define attlist (word-paragraph->attlist p))
    (define runs (word-paragraph->runs p))
    
    (list 'w:p attlist runs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define word-paragraph->attlist : (-> Word-Paragraph Xexpr-AttList)
  (lambda [p]
    null))

(define word-paragraph->runs : (-> Word-Paragraph (Listof Xexpr))
  (lambda [p]
    (for/list : (Listof Xexpr) ([c (in-list (word-paragraph-content p))])
      (cond [(string? c) (list 'w:r null (list (list 'w:t null (list c))))]
            [else (list 'w:r null (list (list 'w:t null (list ""))))]))))
