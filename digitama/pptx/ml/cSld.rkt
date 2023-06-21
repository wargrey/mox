#lang typed/racket/base

(provide (all-defined-out))

(require "../../drawing/ml/main/shape.rkt")

(require "pml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-common-slide-data : PPTX:Common-Slide-Data
  (make-pptx:common-slide-data
   #:spTree (make-pptx:group-shape
             #:grpSpPr (make-mox:group-shape-property)
             #:nvGrpSpPr (pptx-nvisual-property
                          (make-mox:nvisual-canvas-property)
                          (make-mox:nvisual-group-shape-property)
                          (make-pptx:nvisual-application-property)))))
