#lang typed/racket/base

(provide (all-defined-out))
(provide MOX-Stdin MOX-Package)

(require "digitama/package.rkt")
(require "digitama/xlsx/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xlsx-package : (-> MOX-Stdin MOX-Package)
  (lambda [/dev/stdin]
    (mox-input-package /dev/stdin moxml-excel-agent)))
