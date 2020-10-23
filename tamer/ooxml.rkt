#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../docx.rkt"))
(provide (all-from-out "../xlsx.rkt"))
(provide (all-from-out racket/logging racket/path racket/port racket/pretty))

(require "../docx.rkt")
(require "../xlsx.rkt")

(require racket/path)
(require racket/port)
(require racket/logging)
(require racket/pretty)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (#%dir stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (if (path? full) (assert (path-only full) path?) (current-directory)))])))

(define-syntax (#%docx stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (cond [(path? full) (path-replace-extension full #".docx")]
                          [else (current-directory)]))])))

(define-syntax (#%xlsx stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (cond [(path? full) (path-replace-extension full #".xlsx")]
                          [else (current-directory)]))])))
