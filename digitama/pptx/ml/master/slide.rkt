#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require sgml/sax)

(require "../../../drawing/ml/main.rkt")
(require "../pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pptx-slide-master : PPTX-Slide-Master
  ([namespaces : MOX-Namespaces null]
   [preserve? : XML-Boolean 'false]
   [color-map : MOX:Attr:Color-Map default-mox-color-map]
   [layouts : (Listof PPTX:Attr:Slide-Layout-Entry) null])
  #:transparent)

(define empty-slide-master : PPTX-Slide-Master
  (make-pptx-slide-master))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamentals And Markup Language References 19.3.1.42
(define pptx-slide-master-sax-element : (XML-Element-Handler PPTX-Slide-Master)
  (lambda [element depth attrs ?empty ?preserve self]
    (if (and attrs)
        (case element
          [(p:sldMaster)
           (let*-values ([(ns rest) (mox-attributes-extract-xmlns attrs)]
                         [(prvr rest) (xml-attributes-extract rest 'preserve)])
             (remake-pptx-slide-master #:preserve? (or (and prvr (xml:attr-value->boolean prvr)) 'false)
                                       #:namespaces ns
                                       self))]
          [(p:clrMap)
           (let-values ([(clr rest) (extract-mox:attr:color-map attrs)])
             (remake-pptx-slide-master self #:color-map (assert clr)))]
          [(p:sldLayoutId)
           (let-values ([(slid rest) (extract-pptx:attr:slide-layout-entry attrs)])
             (remake-pptx-slide-master self #:layouts (cons (assert slid) (pptx-slide-master-layouts self))))]
          [(p:sldLayoutIdLst) self]
          [else #false])

        #| ETag |#
        (case element
          [(p:sldLayoutIdLst) (remake-pptx-slide-master self #:layouts (reverse (pptx-slide-master-layouts self)))]
          [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-slide-master-sax-handler : (XML-Event-Handlerof PPTX-Slide-Master)
  ((inst make-xml-event-handler PPTX-Slide-Master)
   #:element pptx-slide-master-sax-element))
