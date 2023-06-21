#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main.rkt")
(require "../../drawing/ml/lstStyle.rkt")

(require "pml.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-presentation : PPTX:Presentation (make-pptx:presentation #:notes-size default-positive-2dsize))

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
          [(p:sldMasterIdLst) (remake-pptx:presentation self #:slide-masters (xml-element->slide-master-entries child))]
          [(p:sldIdLst) (remake-pptx:presentation self #:slides (xml-element->slide-entries child))]
          [(p:sldSz)
           (let-values ([(sldsz _) (extract-pptx#slide-size (cadr child) (car child))])
             (remake-pptx:presentation self #:slide-size sldsz))]
          [(p:notesSz)
           (let-values ([(ntsz _) (extract-mox#positive-2dsize (cadr child))])
             (remake-pptx:presentation self #:notes-size ntsz))]
          [(p:defaultTextStyle)
           (let ([lstStyle (xml-element->text-list-style child)])
             (if (not lstStyle) self (remake-pptx:presentation self #:default-text-style lstStyle)))]
          [(p:extLst) (remake-pptx:presentation self #:extension (xml-element->extension-list-modify child))]
          [else self])))

    (when (and (not self)
               (eq? (pptx:presentation-notes-size self)
                    default-positive-2dsize))
      (raise-xml-missing-element-error (car root) 'notesSz))
    
    self))

(define xml-document->presentation : (-> XML-Document PPTX:Presentation)
  (lambda [presentation.xml]
    (xml-document->presentation/text presentation.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->slide-master-entries : (-> XML-Element (Listof PPTX#Slide-Master-Entry))
  (lambda [sldMasterIdLst]
    (reverse
     (for/fold ([seirtne : (Listof PPTX#Slide-Master-Entry) null])
               ([child (in-list (caddr sldMasterIdLst))] #:when (list? child))
       (let-values ([(entry _) (extract-pptx#slide-master-entry (cadr child) (car child))])
         (cons entry seirtne))))))

(define xml-element->slide-entries : (-> XML-Element (Listof PPTX#Slide-Entry))
  (lambda [sldIdLst]
    (reverse
     (for/fold ([seirtne : (Listof PPTX#Slide-Entry) null])
               ([child (in-list (caddr sldIdLst))] #:when (list? child))
       (let-values ([(entry _) (extract-pptx#slide-entry (cadr child) (car child))])
         (cons entry seirtne))))))
