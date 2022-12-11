#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-render-mode : Symbol 'docx)
(define docx-part-tag : Symbol 'gydm:section:docx)
(define docx-suffix : Bytes #".docx")
