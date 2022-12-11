#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/string)

(require "typed/scribble.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scribble-style->values : (-> Element-Style (Values Style-Name Style-Properties))
  (lambda [s]
    (cond [(style? s) (values (style-name s) (style-properties s))]
          [else (values s null)])))

(define scribble-tag-normalize : (-> (List Symbol Any) (Pairof Symbol String))
  (lambda [tag]
    (cons (car tag)
          (let normalize : String ([v : Any (cadr tag)])
            (cond [(string? v) (string-replace v " " "_")]
                  [(symbol? v) (symbol->immutable-string v)]
                  [(list? v) (string-join (map normalize v) ":")]
                  [else (normalize (format "~a" v))])))))
