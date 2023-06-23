#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../../drawing/ml/main/extension.rkt"))

(require "../../dialect.rkt")
(require "../../drawing/ml/main/extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (PPTX-Extension-With D) (MOX+Extension D PPTX:Extension-List))
(define-type (PPTX-Extension-Modify-With D) (MOX+Extension D PPTX:Extension-List-Modify))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element extension-list #:for pptx ())
(define-mox-element extension-list-modify #:for pptx ())
