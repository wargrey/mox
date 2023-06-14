#lang typed/racket/base

(require sgml/rnc)

(require "ooxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.rnc : Path-String
  (or ($1)
      (raise-user-error 'rnc "needs an input file")))

(define rnc : RNC-Grammar (read-rnc-grammar file.rnc))

rnc
