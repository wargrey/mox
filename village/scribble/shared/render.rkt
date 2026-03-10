#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require digimon/date)
(require digimon/scribble)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-story-part : (-> Symbol String Symbol (Pairof String Symbol))
  (lambda [target id name]
    (cons (mox-story-part-name target id name) name)))

(define mox-story-part-name : (-> Symbol String Symbol String)
  (lambda [target id filename]
    (string-append "/" id "/story/"
                   (symbol->immutable-string filename))))

(define mox-sift-property : (-> Symbol (Listof Any) (Values (Listof Any) String String String))
  (λ [id ps]
    (let sift-property ([properties : (Listof Any) ps]
                        [doc-id : String (symbol->immutable-string id)]
                        [doc-version : String ""]
                        [doc-date : String (strftime)]
                        [srehto : (Listof Any) null])
      (if (pair? properties)
          (let-values ([(self rest) (values (car properties) (cdr properties))])
            (cond [(body-id? self) (sift-property rest (string-replace (body-id-value self) #px"[/\\\\]" "-") doc-version doc-date srehto)]
                  [(document-version? self) (sift-property rest doc-id (document-version-text self) doc-date srehto)]
                  [(document-date? self) (sift-property rest doc-id doc-version (document-date-text self) srehto)]
                  [else (sift-property rest doc-id doc-version doc-date (cons self srehto))]))
          (values (reverse srehto) doc-id doc-version doc-date)))))

(define mox-relation-id : (-> Symbol Symbol)
  (lambda [type]
    (define s (symbol->immutable-string type))

    (cond [(not (string-contains? s ".")) (gensym type)]
          [else (let ([ss (string-split s #px"[.]")])
                  (gensym (car ss)))])))
