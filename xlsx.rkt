#lang typed/racket/base

(provide (all-defined-out))
(provide XLSX-StdIn XLSX-Package)

(require "digitama/xlsx/package.rkt")

(define read-xlsx-package : (-> XLSX-StdIn XLSX-Package)
  (lambda [/dev/stdin]
    (xlsx-input-package /dev/stdin)))
