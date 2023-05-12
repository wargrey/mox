#lang typed/racket/base

(provide (all-defined-out))
(provide MOX-Stdin MOX-Package)

(require "digitama/package.rkt")
(require "digitama/docx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-docx-package-for-template : (-> MOX-Stdin MOX-Package-Template)
  (lambda [/dev/stdin]
    (mox-input-package-for-template /dev/stdin moxml-word-agent)))

(define read-docx-package : (-> MOX-Stdin MOX-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-word-agent)))
