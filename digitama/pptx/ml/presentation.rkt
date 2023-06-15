#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require sgml/sax)

(require "../../drawing/ml/main.rkt")
(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pptx-presentation : PPTX-Presentation
  ([namespaces : MOX-Namespaces null]
   [attribute : (Option PPTX:Attr:Presentation) #false]
   [slide-masters : (Listof PPTX:Attr:Slide-Master-Entry) null]
   [slides : (Listof PPTX:Attr:Slide-Entry) null]
   [slide-size : (Option PPTX:Attr:Slide-Size) #false]
   [notes-size : (Pairof Index Index) (cons 0 0)])
  #:transparent)

(define empty-presentation : PPTX-Presentation
  (make-pptx-presentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamentals And Markup Language References 19.2
(define pptx-presentation-sax-element : (XML-Element-Handler PPTX-Presentation)
  (lambda [element depth attrs ?empty ?preserve self]
    (if (and attrs)
        (case element
          [(p:presentation)
           (let*-values ([(ns rest) (mox-attributes-extract-xmlns attrs)]
                         [(pattr rest) (extract-pptx:attr:presentation rest)])
             (remake-pptx-presentation self #:namespaces ns #:attribute pattr))]
          [(p:sldMasterId)
           (let-values ([(mid rest) (extract-pptx:attr:slide-master-entry attrs)])
             (remake-pptx-presentation self #:slide-masters (cons (assert mid) (pptx-presentation-slide-masters self))))]
          [(p:sldId)
           (let-values ([(sid rest) (extract-pptx:attr:slide-entry attrs)])
             (remake-pptx-presentation self #:slides (cons (assert sid) (pptx-presentation-slides self))))]
          [(p:sldSz)
           (let-values ([(sldsz rest) (extract-pptx:attr:slide-size attrs)])
             (remake-pptx-presentation self #:slide-size sldsz))]
          [(p:notesSz)
           (let-values ([(ntsz rest) (mox-attributes-extract-positive-2dsize attrs 'cx 'cy)])
             (remake-pptx-presentation self #:notes-size (or ntsz (pptx-presentation-notes-size self))))]
          [(p:sldMasterIdLst p:sldIdLst) self]
          [else #false])

        #| ETag |#
        (case element
          [(p:sldMasterIdLst) (remake-pptx-presentation self #:slide-masters (reverse (pptx-presentation-slide-masters self)))]
          [(p:sldIdLst) (remake-pptx-presentation self #:slides (reverse (pptx-presentation-slides self)))]
          [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-presentation-sax-handler : (XML-Event-Handlerof PPTX-Presentation)
  ((inst make-xml-event-handler PPTX-Presentation)
   #:element pptx-presentation-sax-element))
