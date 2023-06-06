#lang typed/racket/base

(require "ooxml.rkt")

(require "../digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file.mox : Path-String
  (or ($1)
      (raise-user-error 'moxml "needs an input file")))

(define mox.zip (time (read-pptx-package file.mox)))

(mox-pkg-properties mox.zip)
(mox-pkg-content-types mox.zip)
(mox-pkg-relationships mox.zip)
(mox-pkg-orphans mox.zip)
(mox-theme mox.zip)
