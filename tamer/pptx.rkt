#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.pptx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.pptx")))

(define pptx.zip (time (read-pptx-package file.pptx)))
(define powerpoint.ml (mox-self pptx.zip))

(mox-powerpoint-presentation powerpoint.ml)
(mox-pkg-orphans pptx.zip)
