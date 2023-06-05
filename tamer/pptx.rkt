#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/package.rkt")
(require "../digitama/pptx/moxml.rkt")
(require "../digitama/shared/moxml.rkt")
(require "../digitama/drawing/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.pptx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.pptx")))

(define pptx.zip (time (read-pptx-package file.pptx)))
(define drawing.ml (mox.ml-drawing pptx.zip))
(define shared.ml (mox.ml-shared pptx.zip))
(define powerpoint.ml (mox.ml-document pptx.zip))

(mox.zip-types pptx.zip)
(mox.zip-rels pptx.zip)
(mox.zip-part-rels pptx.zip)
(mox-sharedml-properties shared.ml)
(mox-package-orphans pptx.zip)

;(mox-drawingml-theme drawing.ml)
powerpoint.ml
