#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->color-choice : (-> XML-Element (Option MOX-Color))
  (lambda [ColorChoice]
    #false))
