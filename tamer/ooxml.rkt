#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../docx.rkt"))
(provide (all-from-out "../xlsx.rkt"))
(provide (all-from-out racket/logging racket/file racket/path racket/port racket/pretty))

(require "../docx.rkt")
(require "../xlsx.rkt")

(require racket/logging)
(require racket/file)
(require racket/path)
(require racket/port)
(require racket/pretty)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax ($1 stx)
  (syntax/loc stx
    (let ([argv (current-command-line-arguments)])
      (cond [(and (list? argv) (pair? argv)) (car argv)]
            [(> (vector-length argv) 0) (vector-ref argv 0)]
            [else #false]))))

(define-syntax (#%dir stx)
  (syntax/loc stx
    (let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (if (path? full) (assert (path-only full) path?) (current-directory)))]))))

(define-syntax (#%docx stx)
  (syntax/loc stx
    (let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (cond [(path? full) (path-replace-extension full #".docx")]
                          [else (current-directory)]))]))))

(define-syntax (#%xlsx stx)
  (syntax/loc stx
    (let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (cond [(not rmp) (current-directory)]
            [else (let ([full (resolved-module-path-name rmp)])
                    (cond [(path? full) (path-replace-extension full #".xlsx")]
                          [else (current-directory)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pretty-print-columns 160)
(global-port-print-handler pretty-print)
