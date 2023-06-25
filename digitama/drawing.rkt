#lang typed/racket/base

(provide (all-defined-out))

(require "drawing/ml/main/text.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define text-paragraph-unbox : (-> MOX:Text-Paragraph String)
  (lambda [p]
    (for/fold ([text : String ""])
              ([r (in-list (mox:text-paragraph-r* p))])
      (cond [(mox:text-run? r) (string-append text (mox:text-run-t r))]
            [(mox:text-line-break? r) (string-append text "\n")]
            ; TODO: support the text fields (dynamic content)
            [else text]))))
