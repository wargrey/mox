#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mox-color-map : MOX:Color-Map
  (make-mox:color-map #:attlist
                      (make-mox#color-map #:bg1 'lt1 #:tx1 'dk1 #:bg2 'lt2 #:tx2 'dk2
                                          #:accent1 'accent1 #:accent2 'accent2 #:accent3 'accent3
                                          #:accent4 'accent4 #:accent5 'accent5 #:accent6 'accent6
                                          #:hlink 'hlink #:folHlink 'folHlink)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->color-map : (-> XML-Element MOX:Color-Map)
  (lambda [clrMap]
    (define-values (a:cm _) (extract-mox#color-map (cadr clrMap)))
    (remake-mox:color-map default-mox-color-map #:attlist a:cm)))

(define xml-element->color-map-override : (-> XML-Element (Option MOX-Color-Map-Override))
  (lambda [clrMapOvr]
    (define children : (Listof XML-Element-Children) (caddr clrMapOvr))

    (and (pair? children)
         (let ([child (car children)])
           (and (list? child)
                (case (car child)
                  [(a:masterClrMapping) #true]
                  [(a:overrideClrMapping) (xml-element->color-map child)]
                  [else #false]))))))
