#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/clrMap.rkt")

(require "pml.rkt")
(require "cSld.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide : PPTX:Slide (make-pptx:slide #:cSld default-common-slide-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->slide/text : (-> XML-Document PPTX:Slide)
  (lambda [slide.xml]
    (define root : XML-Element (assert (xml-root-xexpr slide.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (attr _) (extract-pptx#slide rest (car root)))

    (xml-children-filter-fold root mox-slide-fold
                              (remake-pptx:slide #:src slide.xml #:xmlns ns #:attlist attr
                                                 default-slide))))

(define xml-document->slide : (-> XML-Document PPTX:Slide)
  (lambda [master.xml]
    (xml-document->slide/text master.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-slide-fold : (XML-Children-Filter-Fold PPTX:Slide)
  (lambda [child self root]
    (case (car child)
      [(p:cSld) (remake-pptx:slide self #:cSld (xml-element->common-slide-data child))]
      [(p:clrMapOvr)
       (let ([clrMap (xml-element->color-map-override child)])
         (and clrMap (remake-pptx:slide self #:clrMapOvr clrMap)))]
      [(p:extLst) (remake-pptx:slide self #:extLst (xml-element->extension-list-modify child))]
      [else #false])))
