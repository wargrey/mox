#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xlsx-render-mode : Symbol 'xlsx)
(define xlsx-part-tag : Symbol 'gydm:section:xlsx)
(define xlsx-suffix : Bytes #".xlsx")
