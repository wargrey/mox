#lang racket

(provide (all-defined-out))
(provide (all-from-out digimon/tamer))

(require mox/digitama/scribble/package/core)

(require digimon/tamer)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (mox-handbook-title stx)
  (syntax-parse stx #:literals []
    [(_ (~and (~seq (~seq kw:keyword category:expr) ...) (~seq properties ...))
        pre-contents ...)
     (syntax/loc stx
       (handbook-title #:properties (list (make-mox-metainfo properties ...))
                       pre-contents ...))]))

(define-syntax (mox-handbook-title/pkg-desc stx)
  (syntax-parse stx #:literals []
    [(_ (~and (~seq (~seq kw:keyword category:expr) ...) (~seq properties ...))
        pre-contents ...)
     (syntax/loc stx
       (handbook-title/pkg-desc #:properties (list (make-mox-metainfo properties ...))
                                pre-contents ...))]))
