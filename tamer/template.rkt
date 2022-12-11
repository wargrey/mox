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

(define docx.zip (time (read-docx-package-for-template file.docx)))

(hash-keys (mox-package-template-orphans docx.zip))
