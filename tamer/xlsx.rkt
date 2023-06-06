#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/base.rkt")
(require "../digitama/xlsx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.xlsx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.xlsx")))

(define xlsx.zip (time (read-xlsx-package file.xlsx)))
(define excel.ml (mox-self xlsx.zip))

(mox-pkg-content-types xlsx.zip)
(mox-pkg-relationships xlsx.zip)
(mox-pkg-properties xlsx.zip)
(mox-pkg-orphans xlsx.zip)

#;(mox-theme xlsx.zip)
excel.ml
