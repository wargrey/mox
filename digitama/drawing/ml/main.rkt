#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "main/base.rkt" "main/text.rkt"))
(provide (all-from-out "main/color.rkt" "main/fill.rkt"))
(provide (all-from-out "main/shape.rkt" "main/media.rkt"))
(provide (all-from-out "main/matrix.rkt" "main/extension.rkt"))

(require "../../dialect.rkt")
(require "../../shared/ml/common-simple-types.rkt")

(require "main/base.rkt")
(require "main/text.rkt")
(require "main/color.rkt")
(require "main/fill.rkt")
(require "main/matrix.rkt")
(require "main/shape.rkt")
(require "main/media.rkt")
(require "main/extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-point2d : MOX#Point2d
  (make-mox#point2d #:x 0 #:y 0))

(define default-positive-size2d : MOX#Positive-Size2d
  (make-mox#positive-size2d #:cx 0 #:cy 0))
