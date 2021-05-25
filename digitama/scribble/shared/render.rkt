#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require digimon/date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module untyped typed/racket/base
  (require/typed/provide
   scribble/core
   [#:struct document-date ([text : String]) #:extra-constructor-name make-document-date]
   [#:struct document-version ([text : String]) #:extra-constructor-name make-document-version])

  (require/typed/provide
   scribble/html-properties
   [#:struct body-id ([value : String]) #:extra-constructor-name make-body-id]))

(require (submod "." untyped))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-story-part : (-> String Symbol (Pairof String Symbol))
  (lambda [id name]
    (cons (mox-story-part-name id name) name)))

(define mox-story-part-name : (-> String Symbol String)
  (lambda [id filename]
    (string-append "/" id
                   "/story/"
                   (symbol->immutable-string filename))))

(define mox-sift-property : (-> Symbol (Listof Any) (Values (Listof Any) String String String))
  (λ [id ps]
    (let sift-property ([properties : (Listof Any) ps]
                        [doc-id : String (symbol->immutable-string id)]
                        [doc-version : String ""]
                        [doc-date : String (strftime #:locale? #false)]
                        [srehto : (Listof Any) null])
      (cond [(null? properties) (values (reverse srehto) doc-id doc-version doc-date)]
            [else (let-values ([(self rest) (values (car properties) (cdr properties))])
                    (cond [(body-id? self) (sift-property rest (string-replace (body-id-value self) #px"[/\\\\]" "-") doc-version doc-date srehto)]
                          [(document-version? self) (sift-property rest doc-id (document-version-text self) doc-date srehto)]
                          [(document-date? self) (sift-property rest doc-id doc-version (document-date-text self) srehto)]
                          [else (sift-property rest doc-id doc-version doc-date (cons self srehto))]))]))))

(define mox-relation-id : (-> Symbol Symbol)
  (lambda [type]
    (define s (symbol->immutable-string type))

    (cond [(not (string-contains? s ".")) (gensym type)]
          [else (let ([ss (string-split s #px"[.]")])
                  (gensym (car ss)))])))
