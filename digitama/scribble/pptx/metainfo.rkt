#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-render-mode : Symbol 'pptx)
(define pptx-part-tag : Symbol 'gydm:section:pptx)
(define pptx-suffix : Bytes #".pptx")
