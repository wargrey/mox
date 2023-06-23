#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/base.rkt")
(require "main/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-transform2d (make-mox:transform2d))
(define default-group-transform2d (make-mox:group-transform2d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->transform2d : (-> XML-Element (Option MOX:Transform2d))
  (lambda [child]
    (define-values (attlist _) (extract-mox#transform2d (cadr child) (car child)))
    (xml-children-filter-fold child mox-transform2d-fold
                              (and attlist (make-mox:transform2d #:attlist attlist)))))

(define xml-element->group-transform2d : (-> XML-Element (Option MOX:Group-Transform2d))
  (lambda [child]
    (define-values (attlist _) (extract-mox#group-transform2d (cadr child) (car child)))
    (xml-children-filter-fold child mox-group-transform2d-fold
                              (and attlist (make-mox:group-transform2d #:attlist attlist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-transform2d-fold : (XML-Children-Filter-Fold (Option MOX:Transform2d))
  (lambda [child self parent]
    (case (car child)
      [(a:off)
       (let-values ([(pt _) (extract-mox#point2d (cadr child) (car child))])
         (remake-mox:transform2d (or self default-transform2d) #:off pt))]
      [(a:ext)
       (let-values ([(sz _) (extract-mox#positive-size2d (cadr child) (car child))])
         (remake-mox:transform2d (or self default-transform2d) #:ext sz))]
      [else #false])))

(define mox-group-transform2d-fold : (XML-Children-Filter-Fold (Option MOX:Group-Transform2d))
  (lambda [child self parent]
    (case (car child)
      [(a:off)
       (let-values ([(pt _) (extract-mox#point2d (cadr child) (car child))])
         (remake-mox:group-transform2d (or self default-group-transform2d) #:off pt))]
      [(a:chOff)
       (let-values ([(pt _) (extract-mox#point2d (cadr child) (car child))])
         (remake-mox:group-transform2d (or self default-group-transform2d) #:chOff pt))]
      [(a:ext)
       (let-values ([(sz _) (extract-mox#positive-size2d (cadr child) (car child))])
         (remake-mox:group-transform2d (or self default-group-transform2d) #:ext sz))]
      [(a:chExt)
       (let-values ([(sz _) (extract-mox#positive-size2d (cadr child) (car child))])
         (remake-mox:group-transform2d (or self default-group-transform2d) #:chExt sz))]
      [else #false])))
