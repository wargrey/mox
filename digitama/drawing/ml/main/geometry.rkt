#lang typed/racket/base

(provide (all-defined-out))

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-attribute geometry-guide #:for mox
  ([name : String #:<-> xml:attr-value->token]
   [fmla : String #:<-> xml:attr-value->string]))
