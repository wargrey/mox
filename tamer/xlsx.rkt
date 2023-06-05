#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/package.rkt")
(require "../digitama/xlsx/moxml.rkt")
(require "../digitama/shared/moxml.rkt")
(require "../digitama/drawing/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.xlsx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.xlsx")))

(define xlsx.zip (time (read-xlsx-package file.xlsx)))
(define drawing.ml (mox.ml-drawing xlsx.zip))
(define shared.ml (mox.ml-shared xlsx.zip))
(define excel.ml (mox.ml-document xlsx.zip))

(mox.zip-types xlsx.zip)
(mox.zip-rels xlsx.zip)
(mox.zip-part-rels xlsx.zip)
(mox-sharedml-properties shared.ml)
(mox-package-orphans xlsx.zip)

excel.ml
