#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/shape.rkt")

(require "extLst.rkt")
(require "Hyperlink.rkt")
(require "FillProps.rkt")
(require "Transform2D.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-nvisual-canvas-property
  (make-mox:nvisual-canvas-property #:attlist (make-mox#nvisual-canvas-property #:id 0 #:name "")))

(define default-group-shape-property (make-mox:group-shape-property))
(define default-nvisual-group-shape-property (make-mox:nvisual-group-shape-property))

(define default-shape-property (make-mox:shape-property))
(define default-nvisual-shape-property (make-mox:nvisual-shape-property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->nvisual-canvas-property : (-> XML-Element MOX:Nvisual-Canvas-Property)
  (lambda [child]
    (define-values (attlist _) (extract-mox#nvisual-canvas-property (cadr child) (car child)))
    (xml-children-filter-fold child mox-nvisual-canvas-property-fold
                              (make-mox:nvisual-canvas-property #:attlist attlist))))

(define xml-element->nvisual-group-shape-property : (-> XML-Element MOX:Nvisual-Group-Shape-Property)
  (lambda [child]
    (xml-children-filter-fold child mox-nvisual-group-shape-property-fold
                              default-nvisual-group-shape-property)))

(define xml-element->group-shape-property : (-> XML-Element MOX:Group-Shape-Property)
  (lambda [child]
    (define-values (attlist _) (extract-mox#group-shape-property (cadr child) (car child)))
    
    (xml-children-filter-fold child mox-group-shape-property-fold
                              (cond [(not attlist) default-group-shape-property]
                                    [else (make-mox:group-shape-property #:attlist attlist)]))))

(define xml-element->nvisual-shape-property : (-> XML-Element MOX:Nvisual-Shape-Property)
  (lambda [child]
    (xml-children-filter-fold child mox-nvisual-shape-property-fold
                              default-nvisual-shape-property)))

(define xml-element->shape-property : (-> XML-Element MOX:Shape-Property)
  (lambda [child]
    (define-values (attlist _) (extract-mox#shape-property (cadr child) (car child)))
    
    (xml-children-filter-fold child mox-shape-property-fold
                              (cond [(not attlist) default-shape-property]
                                    [else (make-mox:shape-property #:attlist attlist)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-nvisual-canvas-property-fold : (XML-Children-Filter-Fold MOX:Nvisual-Canvas-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:hlinkClick)
       (remake-mox:nvisual-canvas-property self #:hlinkClick (xml-element->hyperlink child))]
      [(a:hlinkHover)
       (remake-mox:nvisual-canvas-property self #:hlinkHover (xml-element->hyperlink child))]
      [(a:extLst)
       (remake-mox:nvisual-canvas-property self #:extLst (xml-element->art-extension-list child))]
      [else #false])))

(define mox-nvisual-group-shape-property-fold : (XML-Children-Filter-Fold MOX:Nvisual-Group-Shape-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:grpSpLocks)
       (let ([locks (xml-element->attribute+art-extension-list child extract-mox#group-shape-locking)])
         (and locks (remake-mox:nvisual-group-shape-property self #:grpSpLocks locks)))]
      [(a:extLst)
       (remake-mox:nvisual-group-shape-property self #:extLst (xml-element->art-extension-list child))]
      [else #false])))

(define mox-group-shape-property-fold : (XML-Children-Filter-Fold MOX:Group-Shape-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:xfrm)
       (let ([grp-trans (xml-element->group-transform2d child)])
         (and grp-trans (remake-mox:group-shape-property self #:xfrm grp-trans)))]
      [(a:extLst)
       (remake-mox:group-shape-property self #:extLst (xml-element->art-extension-list child))]
      [else (cond [(xml-element->fill-property child)
                   => (λ [fill] (remake-mox:group-shape-property self #:fill fill))]
                  [else #false])])))

(define mox-nvisual-shape-property-fold : (XML-Children-Filter-Fold MOX:Nvisual-Shape-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:spLocks)
       (let ([locks (xml-element->attribute+art-extension-list child extract-mox#shape-locking)])
         (and locks (remake-mox:nvisual-shape-property self #:spLocks locks)))]
      [(a:extLst)
       (remake-mox:nvisual-shape-property self #:extLst (xml-element->art-extension-list child))]
      [else #false])))

(define mox-shape-property-fold : (XML-Children-Filter-Fold MOX:Shape-Property)
  (lambda [child self parent]
    (case (car child)
      [(a:xfrm)
       (let ([grp-trans (xml-element->group-transform2d child)])
         (and grp-trans (remake-mox:shape-property self #:xfrm grp-trans)))]
      [(a:extLst)
       (remake-mox:shape-property self #:extLst (xml-element->art-extension-list child))]
      [else (cond [(xml-element->fill-property child)
                   => (λ [fill] (remake-mox:shape-property self #:fill fill))]
                  [else #false])])))

