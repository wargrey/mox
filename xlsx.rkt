#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))

(require "digitama/base.rkt")
(require "digitama/package.rkt")
(require "digitama/xlsx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Excel-Package (MOX-Packageof MOX-Excel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xlsx-package : (-> MOX-Stdin MOX-Excel-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-excel-agent)))
