#lang typed/racket/base

(provide (all-defined-out))

(require sgml/sax)

(require "../../../drawing/ml/main.rkt")
(require "../pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-slide-master : PPTX-Slide-Master (make-pptx-slide-master))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-slide-master-text-sax-element : (XML-Element-Handler PPTX-Slide-Master)
  (lambda [element xpath attlist ?empty ?preserve self]
    (case element
      [(p:sldMaster)
       (cond [(not attlist) self]
             [else (let*-values ([(ns rest) (xml-attributes-extract-xmlns attlist)]
                                 [(maybe-prvr rest) (xml-attributes-extract rest 'preserve)]
                                 [(prvr) (and maybe-prvr (xml:attr-value->boolean maybe-prvr))])
                     (remake-pptx-slide-master self #:namespaces ns #:preserve? (or prvr 'false)))])]
      [(p:sldLayoutId)
       (cond [(not attlist) self]
             [else (let-values ([(slid rest) (extract-pptx:attr:slide-layout-entry attlist)])
                     (remake-pptx-slide-master #:layouts (cons slid (pptx-slide-master-layouts self))
                                               self))])]
      [(p:sldLayoutIdLst)
       (cond [(list? attlist) self]
             [else (remake-pptx-slide-master self #:layouts (reverse (pptx-slide-master-layouts self)))])]
      [(p:clrMap)
       (cond [(not attlist) self]
             [else (let-values ([(clr rest) (extract-mox:attr:color-map attlist)])
                     (remake-pptx-slide-master self #:color-map (make-mox-color-map #:attlist clr)))])]
      [else #false])))

(define pptx-slide-master-sax-element : (XML-Element-Handler PPTX-Slide-Master)
  (lambda [element xpath attlist ?empty ?preserve self]
    (or (pptx-slide-master-text-sax-element element xpath attlist ?empty ?preserve self)
        (case element
          [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-slide-master-text-sax-handler : (XML-Event-Handlerof PPTX-Slide-Master)
  ((inst make-xml-event-handler PPTX-Slide-Master)
   #:element (sax-element-terminator pptx-slide-master-text-sax-element)))

(define pptx-slide-master-sax-handler : (XML-Event-Handlerof PPTX-Slide-Master)
  ((inst make-xml-event-handler PPTX-Slide-Master)
   #:element pptx-slide-master-sax-element))
