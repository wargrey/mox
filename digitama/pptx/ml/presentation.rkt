#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main.rkt")
(require "../../drawing/ml/lstStyle.rkt")

(require "pml.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-presentation : PPTX-Presentation (make-pptx-presentation #:notes-size default-positive-2dsize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->presentation/text : (-> XML-Document PPTX-Presentation)
  (lambda [presentation.xml]
    (define root : XML-Element (assert (xml-root-xexpr presentation.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (pattr _) (extract-pptx:attr:presentation rest (car root)))

    (let transform ([children : (Listof XExpr-Element-Children) (caddr root)]
                    [self : PPTX-Presentation (remake-pptx-presentation default-presentation #:namespaces ns #:attlist pattr)])
      (cond [(pair? children)
             (let-values ([(child rest) (values (car children) (cdr children))])
               (with-asserts ([child list?])
                 (case (car child)
                   [(p:sldMasterIdLst)
                    (transform rest (remake-pptx-presentation self #:slide-masters (xml-element->slide-master-entries child)))]
                   [(p:sldIdLst)
                    (transform rest (remake-pptx-presentation self #:slides (xml-element->slide-entries child)))]
                   [(p:sldSz)
                    (let-values ([(sldsz _) (extract-pptx:attr:slide-size (cadr child) (car child))])
                      (transform rest (remake-pptx-presentation self #:slide-size sldsz)))]
                   [(p:notesSz)
                    (let-values ([(ntsz _) (extract-mox:attr:positive-2dsize (cadr child))])
                      (transform rest (remake-pptx-presentation self #:notes-size ntsz)))]
                   [(p:defaultTextStyle)
                    (let ([lstStyle (xml-element->text-list-style child)])
                      (transform rest (if (not lstStyle) self (remake-pptx-presentation self #:default-text-style lstStyle))))]
                   [(p:extLst) (transform rest (remake-pptx-presentation self #:extension (xml-element->modify-extension-list child)))]
                   [else (transform rest self)])))]
            [(or (not self) (eq? (pptx-presentation-notes-size self) default-positive-2dsize)) (raise-xml-missing-element-error (car root) 'notesSz)]
            [else self]))))

(define xml-document->presentation : (-> XML-Document PPTX-Presentation)
  (lambda [presentation.xml]
    (xml-document->presentation/text presentation.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->slide-master-entries : (-> XML-Element (Listof PPTX:Attr:Slide-Master-Entry))
  (lambda [sldMasterIdLst]
    (let transform ([children : (Listof XExpr-Element-Children) (caddr sldMasterIdLst)]
                    [seirtne : (Listof PPTX:Attr:Slide-Master-Entry) null])
      (if (pair? children)
          (let-values ([(child rest) (values (car children) (cdr children))])
            (with-asserts ([child list?]) ;  p:sldMasterId
              (let-values ([(entry _) (extract-pptx:attr:slide-master-entry (cadr child) (car child))])
                (transform rest (cons entry seirtne)))))
          (reverse seirtne)))))

(define xml-element->slide-entries : (-> XML-Element (Listof PPTX:Attr:Slide-Entry))
  (lambda [sldIdLst]
    (let transform ([children : (Listof XExpr-Element-Children) (caddr sldIdLst)]
                    [seirtne : (Listof PPTX:Attr:Slide-Entry) null])
      (if (pair? children)
          (let-values ([(child rest) (values (car children) (cdr children))])
            (with-asserts ([child list?]) ; p:sldId
              (let-values ([(entry _) (extract-pptx:attr:slide-entry (cadr child) (car child))])
                (transform rest (cons entry seirtne)))))
          (reverse seirtne)))))
