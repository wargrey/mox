#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "color.rkt")

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
