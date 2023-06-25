#lang typed/racket/base

(provide (all-defined-out))

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "base.rkt")
(require "color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-enumeration font-collection-index : Font-Collection-Index #:for mox [major minor none])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element style-matrix-reference #:for mox
  #:attlist
  ([idx : Index #:<-> xml:attr-value->style-matrix-column-index])
  ([color : (Option MOX-Color) #false]))

(define-mox-element font-reference #:for mox
  #:attlist
  ([idx : Font-Collection-Index #:<-> mox:attr-value->font-collection-index])
  ([color : (Option MOX-Color) #false]))
