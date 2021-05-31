#lang typed/racket/base

(provide (all-defined-out))

(require "typed/scribble.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scribble-style->values : (-> Element-Style (Values Style-Name Style-Properties))
  (lambda [s]
    (cond [(style? s) (values (style-name s) (style-properties s))]
          [else (values s null)])))
