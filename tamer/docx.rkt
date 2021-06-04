#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/package.rkt")
(require "../digitama/docx/moxml.rkt")
(require "../digitama/shared/moxml.rkt")
(require "../digitama/drawing/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.docx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.docx")))

(define docx.zip (time (read-docx-package file.docx)))
(define drawing.ml (mox.ml-drawing docx.zip))
(define shared.ml (mox.ml-shared docx.zip))
(define word.ml (assert (mox.ml-document docx.zip) mox-word?))
(define main.ml (mox-word-main word.ml))

(mox.zip-types docx.zip)
(mox.zip-rels docx.zip)
(mox.zip-part-rels docx.zip)
(mox-sharedml-properties shared.ml)
(mox-package-orphans docx.zip)

(mox-drawingml-theme drawing.ml)

(word-document-styles main.ml)
(word-document-entry main.ml)
