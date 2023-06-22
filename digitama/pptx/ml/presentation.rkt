#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main.rkt")
(require "../../drawing/ml/lstStyle.rkt")

(require "pml.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-presentation : PPTX:Presentation (make-pptx:presentation #:notesSz default-positive-size2d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->presentation/text : (-> XML-Document PPTX:Presentation)
  (lambda [presentation.xml]
    (define root : XML-Element (assert (xml-root-xexpr presentation.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (pattr _) (extract-pptx#presentation rest (car root)))

    (define self : PPTX:Presentation
      (for/fold ([self : PPTX:Presentation (remake-pptx:presentation default-presentation #:xmlns ns #:attlist pattr)])
                ([child (in-list (caddr root))] #:when (list? child))
        (case (car child)
          [(p:sldMasterIdLst)
           (remake-pptx:presentation self #:sldMasterIdLst (xml-empty-children->list child extract-pptx#slide-master-entry))]
          [(p:sldIdLst)
           (remake-pptx:presentation self #:sldIdLst (xml-empty-children->list child extract-pptx#slide-entry))]
          [(p:sldSz)
           (let-values ([(sldsz _) (extract-pptx#slide-size (cadr child) (car child))])
             (remake-pptx:presentation self #:sldSz sldsz))]
          [(p:notesSz)
           (let-values ([(ntsz _) (extract-mox#positive-size2d (cadr child))])
             (remake-pptx:presentation self #:notesSz ntsz))]
          [(p:defaultTextStyle)
           (let ([lstStyle (xml-element->text-list-style child)])
             (if (not lstStyle) self (remake-pptx:presentation self #:defaultTextStyle lstStyle)))]
          [(p:extLst) (remake-pptx:presentation self #:extLst (xml-element->extension-list-modify child))]
          [else self])))

    (when (and (not self)
               (eq? (pptx:presentation-notesSz self)
                    default-positive-size2d))
      (raise-xml-missing-element-error (car root) 'notesSz))
    
    self))

(define xml-document->presentation : (-> XML-Document PPTX:Presentation)
  (lambda [presentation.xml]
    (xml-document->presentation/text presentation.xml)))
