#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main/shape.rkt")

(require "../../drawing/ml/SpPr.rkt")

(require "pml.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-nvisual-application-property (make-pptx:nvisual-application-property))

(define default-common-slide-data : PPTX:Common-Slide-Data
  (make-pptx:common-slide-data
   #:spTree (make-pptx:group-shape
             #:grpSpPr default-group-shape-property
             #:nvGrpSpPr (pptx-nvisual-property
                          default-nvisual-canvas-property
                          default-nvisual-group-shape-property
                          default-nvisual-application-property))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->common-slide-data : (-> XML-Element PPTX:Common-Slide-Data)
  (lambda [cSld]
    (xml-children-filter-fold cSld mox-common-slide-data-fold default-common-slide-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-common-slide-data-fold : (XML-Children-Filter-Fold PPTX:Common-Slide-Data)
  (lambda [child self parent]
    (case (car child)
      [(p:spTree)
       (let ([grpSp (xml-element->group-shape child)])
         (and grpSp (remake-pptx:common-slide-data self #:spTree grpSp)))]
      [(p:extLst) (remake-pptx:common-slide-data self #:extLst (xml-element->extension-list child))]
      [else #false])))

(define mox-group-shape-fold : (XML-Children-Filter-Fold PPTX:Group-Shape)
  (lambda [child self parent]
    (case (car child)
      [(p:grpSp)
       (let ([sp (xml-element->group-shape child)])
         (remake-pptx:group-shape self #:children (cons sp (pptx:group-shape-children self))))]
      [(p:grpSpPr)
       (let ([spPr (xml-element->group-shape-property child)])
         (and spPr (remake-pptx:group-shape self #:grpSpPr spPr)))]
      [(p:nvGrpSpPr)
       (let ([nvprops (xml-element->nvisual-props child 'p:cNvGrpSpPr xml-element->nvisual-group-shape-property)])
         (and nvprops (remake-pptx:group-shape self #:nvGrpSpPr nvprops)))]
      [(p:extLst) (remake-pptx:group-shape self #:extLst (xml-element->extension-list-modify child))]
      [else (remake-pptx:group-shape self #:src child)])))

(define mox-nvisual-application-property-fold : (XML-Children-Filter-Fold PPTX:Nvisual-Application-Property)
  (lambda [child self parent]
    (case (car child)
      [(p:ph)
       (let ([ph (xml-element->attribute+extension-list-modify child extract-pptx#placeholder)])
         (and ph (remake-pptx:nvisual-application-property self #:ph ph)))]
      [(p:extLst)
       (remake-pptx:nvisual-application-property self #:extLst (xml-element->extension-list child))]
      [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->group-shape : (-> XML-Element PPTX:Group-Shape)
  (lambda [child]
    (xml-children-filter-fold child mox-group-shape-fold
                              (pptx:common-slide-data-spTree default-common-slide-data))))

(define xml-element->nvisual-application-property : (-> XML-Element PPTX:Nvisual-Application-Property)
  (lambda [child]
    (define-values (attlist _) (extract-pptx#nvisual-application-property (cadr child) (car child)))
    (xml-children-filter-fold child mox-nvisual-application-property-fold
                              (cond [(not attlist) default-nvisual-application-property]
                                    [else (make-pptx:nvisual-application-property #:attlist attlist)]))))

(define #:forall (T) xml-element->nvisual-props : (-> XML-Element Symbol (-> XML-Element T)
                                                      (PPTX-NVisual-Property T))
  (lambda [nvPrs mox:idPr xml-element->property]
    (let fold ([children : (Listof XML-Element-Children) (caddr nvPrs)]
               [nvGrpSpPr : (Option MOX:Nvisual-Canvas-Property) #false]
               [nvPr : (Option PPTX:Nvisual-Application-Property) #false]
               [nvIdPr : (Option T) #false])
      (if (pair? children)
          (let-values ([(self rest) (values (car children) (cdr children))])
            (if (list? self)
                (let ([tag (car self)])
                  (cond [(eq? tag 'p:cNvPr) (fold rest (xml-element->nvisual-canvas-property self) nvPr nvIdPr)]
                        [(eq? tag 'p:nvPr) (fold rest nvGrpSpPr (xml-element->nvisual-application-property self) nvIdPr)]
                        [(eq? tag mox:idPr) (fold rest nvGrpSpPr nvPr (xml-element->property self))]
                        [else (fold rest nvGrpSpPr nvPr nvIdPr)]))
                (fold rest nvGrpSpPr nvPr nvIdPr)))

          ;;; NOTE
          ; By design, all these three properties should exist,
          ;  even though all optional attributes and children are not present.
          (pptx-nvisual-property (assert nvGrpSpPr) (assert nvIdPr) (assert nvPr))))))
