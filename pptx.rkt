#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))

(require "digitama/base.rkt")
(require "digitama/package.rkt")
(require "digitama/pptx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-PowerPoint-Package (MOX-Packageof MOX-PowerPoint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-pptx-package : (-> MOX-Stdin MOX-PowerPoint-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-power-point-agent)))
