#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/docx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.docx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.docx")))

(define docx.zip (time (read-docx-package file.docx)))
(define word.ml (mox-document docx.zip))
(define main.ml (mox-word-main word.ml))

(word-document-styles main.ml)
(word-document-numbering main.ml)
(word-document-entry main.ml)
