#lang typed/racket/base

(provide (all-defined-out))
(provide MOX-Stdin)

(require "digitama/package.rkt")
(require "digitama/docx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Word-Package (MOX-Packageof MOX-Word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-docx-package-for-template : (-> MOX-Stdin MOX-Package-Template)
  (lambda [/dev/stdin]
    (mox-input-package-for-template /dev/stdin moxml-word-agent)))

(define read-docx-package : (-> MOX-Stdin MOX-Word-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-word-agent)))
