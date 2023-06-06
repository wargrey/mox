#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/base.rkt")
(require "../digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.pptx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.pptx")))

(define pptx.zip (time (read-pptx-package file.pptx)))
(define powerpoint.ml (mox-self pptx.zip))

(mox-pkg-content-types pptx.zip)
(mox-pkg-relationships pptx.zip)
(mox-pkg-properties pptx.zip)
(mox-pkg-orphans pptx.zip)

#;(mox-theme pptx.zip)
powerpoint.ml
