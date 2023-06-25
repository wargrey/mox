#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../../drawing/ml/TextListStyle.rkt")
(require "../../../drawing/ml/clrMap.rkt")

(require "../pml.rkt")
(require "../cSld.rkt")
(require "../extLst.rkt")

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

    (xml-children-fold root mox-slide-master-fold
                       (remake-pptx:slide-master #:xmlns ns #:attlist prvr
                                                 default-slide-master))))

(define xml-document->slide-master : (-> XML-Document PPTX:Slide-Master)
  (lambda [master.xml]
    (xml-document->slide-master/text master.xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-slide-master-fold : (XML-Children-Fold PPTX:Slide-Master)
  (lambda [child self root]
    (case (car child)
      [(p:cSld)
       (remake-pptx:slide-master self #:cSld (xml-element->common-slide-data child))]
      [(p:sldLayoutIdLst)
       (remake-pptx:slide-master self #:sldLayoutIdLst (xml-empty-children-map child extract-pptx#slide-layout-entry))]
      [(p:clrMap)
       (remake-pptx:slide-master self #:clrMap (xml-element->color-map child))]
      [(p:extLst)
       (remake-pptx:slide-master self #:extLst (xml-element->extension-list-modify child))]
      [(p:txStyles)
       (remake-pptx:slide-master self #:txStyles (xml-children-filter-fold child mox-slide-master-text-styles-fold #false))]
      [else self])))
  
(define mox-slide-master-text-styles-fold : (XML-Children-Fold (Option PPTX:Slide-Master-Text-Styles))
  (lambda [child self parent]
    (case (car child)
      [(p:titleStyle)
       (let ([lstStyle (xml-element->text-list-style child)])
         (and lstStyle
              (remake-pptx:slide-master-text-styles #:titleStyle lstStyle
                                                    (or self default-text-styles))))]
      [(p:bodyStyle)
       (let ([lstStyle (xml-element->text-list-style child)])
         (and lstStyle
              (remake-pptx:slide-master-text-styles #:bodyStyle lstStyle
                                                    (or self default-text-styles))))]
      [(p:otherStyle)
       (let ([lstStyle (xml-element->text-list-style child)])
         (and lstStyle
              (remake-pptx:slide-master-text-styles #:otherStyle lstStyle
                                                    (or self default-text-styles))))]
      [(p:extLst)
       (remake-pptx:slide-master-text-styles #:extLst (xml-element->extension-list child)
                                             (or self default-text-styles))]
      [else self])))
