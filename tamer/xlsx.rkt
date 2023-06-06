#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/xlsx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.xlsx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.xlsx")))

(define xlsx.zip (time (read-xlsx-package file.xlsx)))
(define excel.ml (mox-self xlsx.zip))

excel.ml
