#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../../drawing/ml/main.rkt")
(require "../../../drawing/ml/lstStyle.rkt")

(require "../pml.rkt")
(require "../cSld.rkt")
(require "../extLst.rkt")

(require "../../../drawing/ml/main/clrMap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-text-styles : PPTX:Slide-Master-Text-Styles (make-pptx:slide-master-text-styles))
(define default-slide-master : PPTX:Slide-Master
  (make-pptx:slide-master #:clrMap default-mox-color-map
                          #:cSld default-common-slide-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->slide-master/text : (-> XML-Document PPTX:Slide-Master)
  (lambda [master.xml]
    (define root : XML-Element (assert (xml-root-xexpr master.xml)))
    (define-values (ns rest) (xml-attributes-extract-xmlns (cadr root)))
    (define-values (prvr _) (extract-pptx#slide-master rest (car root)))

    (for/fold ([self : PPTX:Slide-Master (remake-pptx:slide-master default-slide-master #:xmlns ns #:attlist prvr)])
              ([child (in-list (caddr root))] #:when (list? child))
      (case (car child)
        [(p:sldLayoutIdLst) (remake-pptx:slide-master self #:sldLayoutIdLst (xml-element->slide-layout-entries child))]
        [(p:clrMap) (remake-pptx:slide-master self #:clrMap (xml-element->color-map child))]
        [(p:extLst) (remake-pptx:slide-master self #:extLst (xml-element->extension-list-modify child))]
        [(p:txStyles) (let ([txStyles (xml-element->slide-master-text-styles child)])
                        (if txStyles (remake-pptx:slide-master self #:txStyles txStyles) self))]
        #;[(or (not self) (eq? (pptx-presentation-notes-size self) default-positive-2dsize)) (raise-xml-missing-element-error (car root) 'notesSz)]
        [else self]))))

(define xml-document->slide-master : (-> XML-Document PPTX:Slide-Master)
  (lambda [master.xml]
    (xml-document->slide-master/text master.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->slide-layout-entries : (-> XML-Element (Listof PPTX#Slide-Layout-Entry))
  (lambda [sldMasterIdLst]
    (reverse
     (for/fold ([seirtne : (Listof PPTX#Slide-Layout-Entry) null])
               ([child (caddr sldMasterIdLst)] #:when (list? child))
       (let-values ([(entry _) (extract-pptx#slide-layout-entry (cadr child) (car child))])
         (cons entry seirtne))))))

(define xml-element->slide-master-text-styles : (-> XML-Element (Option PPTX:Slide-Master-Text-Styles))
  (lambda [txStyles]
    (for/fold ([self : (Option PPTX:Slide-Master-Text-Styles) #false])
              ([child (caddr txStyles)] #:when (list? child))
      (case (car child)
        [(a:titleStyle)
         (let ([lstStyle (xml-element->text-list-style child)])
           (cond [(not lstStyle) self]
                 [else (remake-pptx:slide-master-text-styles #:titleStyle lstStyle
                                                             (or self default-text-styles))]))]
        [(a:bodyStyle)
         (let ([lstStyle (xml-element->text-list-style child)])
           (cond [(not lstStyle) self]
                 [else (remake-pptx:slide-master-text-styles #:bodyStyle lstStyle
                                                             (or self default-text-styles))]))]
        [(a:other)
         (let ([lstStyle (xml-element->text-list-style child)])
           (cond [(not lstStyle) self]
                 [else (remake-pptx:slide-master-text-styles #:otherStyle lstStyle
                                                             (or self default-text-styles))]))]
        [(p:extLst)
         (remake-pptx:slide-master-text-styles #:extLst (xml-element->extension-list child)
                                               (or self default-text-styles))]
        [else self]))))
