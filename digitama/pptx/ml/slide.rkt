#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main.rkt")

(require "pml.rkt")
(require "cSld.rkt")
(require "extLst.rkt")

(require "../../drawing/ml/main/clrMap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide : PPTX:Slide (make-pptx:slide #:cSld default-common-slide-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->slide/text : (-> XML-Document PPTX:Slide)
  (lambda [master.xml]
    (define root : XML-Element (assert (xml-root-xexpr master.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (sattr _) (extract-pptx#slide rest (car root)))

    (for/fold ([self : PPTX:Slide (remake-pptx:slide default-slide #:xmlns ns #:attlist sattr)])
              ([child (in-list (caddr root))] #:when (list? child))
      (case (car child)
        [(p:clrMapOvr)
         (let ([clrMap (xml-element->color-map-override child)])
           (if (not clrMap) self (remake-pptx:slide self #:clrMapOvr clrMap)))]
        [(p:extLst) (remake-pptx:slide self #:extLst (xml-element->extension-list-modify child))]
        #;[(or (not self) (eq? (pptx-presentation-notes-size self) default-positive-2dsize)) (raise-xml-missing-element-error (car root) 'notesSz)]
        [else self]))))

(define xml-document->slide : (-> XML-Document PPTX:Slide)
  (lambda [master.xml]
    (xml-document->slide/text master.xml)))
