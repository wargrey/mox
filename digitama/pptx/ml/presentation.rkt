#lang typed/racket/base

(provide (all-defined-out))

(require sgml/sax)

(require "../../drawing/ml/main.rkt")
(require "../../drawing/ml/lstStyle.rkt")
(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-presentation : PPTX-Presentation (make-pptx-presentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-presentation-text-sax-element : (XML-Element-Handler PPTX-Presentation)
  (lambda [element xpath attlist ?empty ?preserve self]
    (case element
      [(p:presentation)
       (if (list? attlist)
           (let*-values ([(ns rest) (xml-attributes-extract-xmlns attlist)]
                         [(pattr rest) (extract-pptx:attr:presentation rest element)])
             (remake-pptx-presentation self #:namespaces ns #:attlist pattr))
           self)]
      [(p:sldMasterId)
       (if (list? attlist)
           (let-values ([(mid rest) (extract-pptx:attr:slide-master-entry attlist element)])
             (remake-pptx-presentation self #:slide-masters (cons (assert mid) (pptx-presentation-slide-masters self))))
           (remake-pptx-presentation self #:slide-masters (reverse (pptx-presentation-slide-masters self))))]
      [(p:sldId)
       (if (list? attlist)
           (let-values ([(sid rest) (extract-pptx:attr:slide-entry attlist element)])
             (remake-pptx-presentation self #:slides (cons (assert sid) (pptx-presentation-slides self))))
           (remake-pptx-presentation self #:slides (reverse (pptx-presentation-slides self))))]
      [(p:sldSz)
       (cond [(not attlist) self]
             [else (let-values ([(sldsz rest) (extract-pptx:attr:slide-size attlist element)])
                     (remake-pptx-presentation self #:slide-size sldsz))])]
      [(p:notesSz)
       (cond [(not attlist) self]
             [else (let-values ([(ntsz rest) (mox-attributes-extract-positive-2dsize attlist 'cx 'cy)])
                     (remake-pptx-presentation self #:notes-size (or ntsz (pptx-presentation-notes-size self))))])]
      [(p:defaultTextStyle p:sldMasterIdLst p:sldIdLst) self]
      [else #false])))

(define pptx-presentation-sax-element : (XML-Element-Handler PPTX-Presentation)
  (lambda [element xpath attlist ?empty ?preserve self]
    (or (pptx-presentation-text-sax-element element xpath attlist ?empty ?preserve self)
        
        (and (memq 'p:defaultTextStyle xpath)
             (let* ([maybe-lstStyle (pptx-presentation-default-text-style self)]
                    [lstStyle (or maybe-lstStyle (and (list? attlist) default-text-list-style))]
                    [lstStyle++ (and lstStyle (mox-list-style-sax-element element xpath attlist ?empty ?preserve lstStyle))])
               (and lstStyle++ (remake-pptx-presentation self #:default-text-style lstStyle++)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-presentation-text-sax-handler : (XML-Event-Handlerof PPTX-Presentation)
  ((inst make-xml-event-handler PPTX-Presentation)
   #:element (sax-element-terminator pptx-presentation-text-sax-element)))

(define pptx-presentation-sax-handler : (XML-Event-Handlerof PPTX-Presentation)
  ((inst make-xml-event-handler PPTX-Presentation)
   #:element pptx-presentation-sax-element))
