#lang typed/racket/base

(provide (all-defined-out))

(require sgml/sax)

(require "../../drawing/ml/main.rkt")
(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide : PPTX-Slide (make-pptx-slide))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-slide-text-sax-element : (XML-Element-Handler PPTX-Slide)
  (lambda [element xpath attlist ?empty ?preserve self]
    (case element
      [(p:sld)
       (cond [(not attlist) self]
             [else (let*-values ([(ns rest) (xml-attributes-extract-xmlns attlist)]
                                 [(a:sld rest) (extract-pptx:attr:slide rest)])
                     (remake-pptx-slide self #:namespaces ns #:attlist a:sld))])]
      [(p:sldLayoutId)
       (cond [(not attlist) self]
             [else (let-values ([(slid rest) (extract-pptx:attr:slide-layout-entry attlist)])
                     (remake-pptx-slide #:layouts (cons (assert slid) (pptx-slide-layouts self))
                                        self))])]
      [(p:sldLayoutIdLst)
       (cond [(list? attlist) self]
             [else (remake-pptx-slide self #:layouts (reverse (pptx-slide-layouts self)))])]

      ; color map
      [(p:clrMapOvr) self]
      [(a:masterClrMapping) (remake-pptx-slide self #:color-map #true)]
      [(a:overrideClrMapping)
       (cond [(not attlist) self]
             [else (let-values ([(a:cm rest) (extract-mox:attr:color-map attlist)])
                     (remake-pptx-slide self #:color-map (make-mox-color-map #:attlist (assert a:cm))))])]

      [else #false])))

(define pptx-slide-sax-element : (XML-Element-Handler PPTX-Slide)
  (lambda [element xpath attlist ?empty ?preserve self]
    (or (pptx-slide-text-sax-element element xpath attlist ?empty ?preserve self)
        (case element
          [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-slide-text-sax-handler : (XML-Event-Handlerof PPTX-Slide)
  ((inst make-xml-event-handler PPTX-Slide)
   #:element (values #;sax-element-terminator pptx-slide-text-sax-element)))

(define pptx-slide-sax-handler : (XML-Event-Handlerof PPTX-Slide)
  ((inst make-xml-event-handler PPTX-Slide)
   #:element pptx-slide-sax-element))
