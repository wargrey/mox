#lang typed/racket/base

(provide (all-defined-out))
(provide MOX-StdIn MOX-Package)

(require "digitama/package.rkt")
(require "digitama/docx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-docx-package : (-> MOX-StdIn MOX-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-word-agent)))
