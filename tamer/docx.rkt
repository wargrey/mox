#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/package.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.docx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.docx")))

(define docx.zip (time (read-docx-package file.docx)))

docx.zip

(mox.zip-types docx.zip)
