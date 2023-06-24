#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/shape.rkt")
(require "main/hyperlink.rkt")

(require "MediaFile.rkt")
(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->hyperlink : (-> XML-Element MOX:Hyperlink)
  (lambda [child]
    (define-values (attlist _) (extract-mox#hyperlink (cadr child) (car child)))
    (xml-children-filter-fold child mox-hyperlink-fold
                              (make-mox:hyperlink #:attlist attlist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-hyperlink-fold : (XML-Children-Filter-Fold MOX:Hyperlink)
  (lambda [child self parent]
    (case (car child)
      [(a:snd) (remake-mox:hyperlink self #:snd (xml-element->embedded-file child))]
      [(a:extLst) (remake-mox:hyperlink self #:extLst (xml-element->art-extension-list child))]
      [else #false])))
