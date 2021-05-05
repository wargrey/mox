#lang typed/racket/base

(provide (all-defined-out) Info-Ref)

(require typed/setup/getinfo)

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Render (-> String Info-Ref Any))

(struct mox-format
  ([name : Symbol]
   [render : MOX-Render]
   [description : String])
  #:constructor-name make-mox-format
  #:type-name MOX-Format
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define devimon-make-format : (-> #:name Symbol #:render MOX-Render #:desc String MOX-Format)
  (lambda [#:name name #:render render #:desc desc]
    (make-mox-format name render desc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-format-rootdir : (-> Path)
  (lambda []
    (collection-file-path "format" "mox" "digivice" "devimon")))

(define mox-format-ref : (-> Symbol (Option MOX-Format))
  (lambda [frmtx]
    (define strformat : String (symbol->immutable-string frmtx))
    (define format.rkt : Path (build-path (mox-format-rootdir) (string-append strformat ".rkt")))

    (or (and (file-exists? format.rkt)
             (let ([heuristic-sym (string->symbol (string-append strformat "-format"))])
               (define maybe-format (dynamic-require format.rkt heuristic-sym))
               (cond [(and (mox-format? maybe-format) (eq? (mox-format-name maybe-format) frmtx)) maybe-format]
                     [else #false])))
        (hash-ref (mox-list-formats) frmtx (λ [] #false)))))

(define mox-list-formats : (-> (Immutable-HashTable Symbol MOX-Format))
  (lambda []
    (for/fold ([Formats : (Immutable-HashTable Symbol MOX-Format) (make-immutable-hasheq)])
              ([format.rkt (in-directory (mox-format-rootdir))] #:when (regexp-match? #px".rkt$" format.rkt))
      (dynamic-require format.rkt #false)
      (parameterize ([current-namespace (module->namespace format.rkt)])
        (for/fold ([formats : (Immutable-HashTable Symbol MOX-Format) Formats])
                  ([sym (in-list (namespace-mapped-symbols))])
          (define maybe-format (namespace-variable-value sym #false (λ _ #false)))
          (cond [(not (mox-format? maybe-format)) formats]
                [else (hash-set formats (mox-format-name maybe-format) maybe-format)]))))))
