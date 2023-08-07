#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.pptx : Path-String
  (or ($1)
      (build-path (#%dir) "encrypted.pptx")))

(define pptx.zip (time (read-pptx-package-for-text file.pptx)))

(define powerpoint.ml (mox-self pptx.zip))

(mox-powerpoint-presentation powerpoint.ml)
(mox-powerpoint-slide-masters powerpoint.ml)
(mox-powerpoint-slides powerpoint.ml)

(pptx-extract-paragraphs-from-slide-master pptx.zip)
(pptx-extract-paragraphs-from-slide pptx.zip)