#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.pptx : Path-String
  (or ($1)
      (build-path (#%dir) "tamer.pptx")))

(define pptx.zip
  (parameterize ([default-sax-event-postfilter sax-handler/xml-writer])
    (time (read-pptx-package-for-text file.pptx))))

(define powerpoint.ml (mox-self pptx.zip))

(mox-pkg-orphans pptx.zip)
(mox-powerpoint-presentation powerpoint.ml)
(mox-powerpoint-slide-masters powerpoint.ml)
(mox-powerpoint-slides powerpoint.ml)
