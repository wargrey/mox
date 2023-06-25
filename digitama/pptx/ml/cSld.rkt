#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "../../drawing/ml/main/shape.rkt")

(require "../../drawing/ml/SpPr.rkt")
(require "../../drawing/ml/TextBody.rkt")
(require "../../drawing/ml/MediaFile.rkt")
(require "../../drawing/ml/Reference.rkt")

(require "pml.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-nvisual-application-property (make-pptx:nvisual-application-property))

(define default-group-shape
  (make-pptx:group-shape #:grpSpPr default-group-shape-property
                         #:nvGrpSpPr (pptx-nvisual-property
                                      default-nvisual-canvas-property
                                      default-nvisual-group-shape-property
                                      default-nvisual-application-property)))

(define default-shape
  (make-pptx:shape #:spPr default-shape-property
                   #:nvSpPr (pptx-nvisual-property
                                default-nvisual-canvas-property
                                default-nvisual-shape-property
                                default-nvisual-application-property)))

(define default-common-slide-data : PPTX:Common-Slide-Data
  (make-pptx:common-slide-data #:spTree default-group-shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->common-slide-data : (-> XML-Element PPTX:Common-Slide-Data)
  (lambda [cSld]
    (define-values (attlst _) (extract-pptx#common-slide-data (cadr cSld) (car cSld)))
    
    (xml-children-filter-fold cSld mox-common-slide-data-fold
                              (if (and attlst)
                                  (make-pptx:common-slide-data #:attlist attlst
                                                               #:spTree default-group-shape)
                                  default-common-slide-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-common-slide-data-fold : (XML-Children-Filter-Fold PPTX:Common-Slide-Data)
  (lambda [child self parent]
    (case (car child)
      [(p:spTree) (remake-pptx:common-slide-data self #:spTree (xml-element->group-shape child))]
      [(p:extLst) (remake-pptx:common-slide-data self #:extLst (xml-element->extension-list child))]
      [else #false])))

(define mox-group-shape-fold : (XML-Children-Filter-Fold PPTX:Group-Shape)
  (lambda [child self parent]
    (case (car child)
      [(p:sp)
       (remake-pptx:group-shape self #:children (cons (xml-element->shape child)
                                                      (pptx:group-shape-children self)))]
      [(p:grpSp)
       (remake-pptx:group-shape self #:children (cons (xml-element->group-shape child)
                                                      (pptx:group-shape-children self)))]
      [(p:grpSpPr)
       (remake-pptx:group-shape self #:grpSpPr (xml-element->group-shape-property child))]
      [(p:nvGrpSpPr)
       (let ([nvprops (xml-element->nvisual-props child 'p:cNvGrpSpPr
                                                  xml-element->nvisual-group-shape-property)])
         (remake-pptx:group-shape self #:nvGrpSpPr nvprops))]
      [(p:extLst) (remake-pptx:group-shape self #:extLst (xml-element->extension-list-modify child))]
      [else #false])))

(define mox-shape-fold : (XML-Children-Filter-Fold PPTX:Shape)
  (lambda [child self parent]
    (case (car child)
      [(p:nvSpPr)
       (let ([nvprops (xml-element->nvisual-props child 'p:cNvSpPr xml-element->nvisual-shape-property)])
         (remake-pptx:shape self #:nvSpPr nvprops))]
      [(p:txBody) (remake-pptx:shape self #:txBody (xml-element->text-body child))]
      [(p:style) (remake-pptx:shape self #:style (xml-element->shape-style child))]
      [(p:spPr) (remake-pptx:shape self #:spPr (xml-element->shape-property child))]
      [(p:extLst) (remake-pptx:shape self #:extLst (xml-element->extension-list-modify child))]
      [else #false])))

(define mox-nvisual-application-property-fold : (XML-Children-Filter-Fold PPTX:Nvisual-Application-Property)
  (lambda [child self parent]
    (case (car child)
      [(p:ph)
       (let ([ph (xml-element->attribute+extension-list-modify child extract-pptx#placeholder)])
         (and ph (remake-pptx:nvisual-application-property self #:ph ph)))]
      [(p:extLst)
       (remake-pptx:nvisual-application-property self #:extLst (xml-element->extension-list child))]
      [else (let ([mfile (xml-element->media-file child)])
              (and mfile (remake-pptx:nvisual-application-property self #:media mfile)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->group-shape : (-> XML-Element PPTX:Group-Shape)
  (lambda [child]
    (define grp-shape : PPTX:Group-Shape
      (xml-children-filter-fold child mox-group-shape-fold default-group-shape))

    (remake-pptx:group-shape grp-shape #:children (reverse (pptx:group-shape-children grp-shape)))))

(define xml-element->shape : (-> XML-Element PPTX:Shape)
  (lambda [child]
    (define-values (attlist _) (extract-pptx#shape (cadr child) (car child)))
    (xml-children-filter-fold child mox-shape-fold
                              (cond [(not attlist) default-shape]
                                    [else (remake-pptx:shape #:attlist attlist
                                                             default-shape)]))))

(define xml-element->nvisual-application-property : (-> XML-Element PPTX:Nvisual-Application-Property)
  (lambda [child]
    (define-values (attlist _) (extract-pptx#nvisual-application-property (cadr child) (car child)))
    (xml-children-filter-fold child mox-nvisual-application-property-fold
                              (cond [(not attlist) default-nvisual-application-property]
                                    [else (make-pptx:nvisual-application-property #:attlist attlist)]))))

(define #:forall (T) xml-element->nvisual-props : (-> XML-Element Symbol (-> XML-Element T)
                                                      (PPTX-NVisual-Property T))
  (lambda [nvPrs mox:idPr xml-element->property]
    (let fold ([children : XML-Element-Children (caddr nvPrs)]
               [nvGrpSpPr : (Option MOX:Nvisual-Canvas-Property) #false]
               [nvPr : (Option PPTX:Nvisual-Application-Property) #false]
               [nvIdPr : (Option T) #false])
      (if (pair? children)
          (let-values ([(self rest) (values (car children) (cdr children))])
            (if (list? self)
                (let ([tag (car self)])
                  (cond [(eq? tag 'p:cNvPr)
                         (fold rest (xml-element->nvisual-canvas-property self) nvPr nvIdPr)]
                        [(eq? tag 'p:nvPr)
                         (fold rest nvGrpSpPr (xml-element->nvisual-application-property self) nvIdPr)]
                        [(eq? tag mox:idPr)
                         (fold rest nvGrpSpPr nvPr (xml-element->property self))]
                        [else (fold rest nvGrpSpPr nvPr nvIdPr)]))
                (fold rest nvGrpSpPr nvPr nvIdPr)))

          ;;; NOTE
          ; By design, all these three properties should exist,
          ;  even though all optional attributes and children are not present.
          (pptx-nvisual-property (or nvGrpSpPr (raise-xml-missing-element-error (car nvPrs) 'p:cNvPr))
                                 (or nvIdPr (raise-xml-missing-element-error (car nvPrs) mox:idPr))
                                 (or nvPr (raise-xml-missing-element-error (car nvPrs) 'p:nvPr)))))))
