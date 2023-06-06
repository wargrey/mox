#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/base.rkt")
(require "../digitama/docx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.docx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.docx")))

(define docx.zip (time (read-docx-package file.docx)))
(define word.ml (mox-document docx.zip))
(define main.ml (mox-word-main word.ml))

(mox-pkg-content-types docx.zip)
(mox-pkg-relationships docx.zip)
(mox-pkg-properties docx.zip)
(mox-pkg-orphans docx.zip)

(word-document-styles main.ml)
(word-document-numbering main.ml)
(word-document-entry main.ml)

(mox-theme docx.zip)
