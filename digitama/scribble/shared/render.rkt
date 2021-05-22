#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require digimon/archive)

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
(define mox-sift-property : (-> (Listof Any) (Values (Listof Any) String String String))
  (λ [ps]
    (let sift-property ([properties : (Listof Any) ps]
                        [doc-id : String ""]
                        [doc-version : String ""]
                        [doc-date : String ""]
                        [srehto : (Listof Any) null])
      (cond [(null? properties) (values (reverse srehto) doc-id doc-version doc-date)]
            [else (let-values ([(self rest) (values (car properties) (cdr properties))])
                    (cond [(body-id? self) (sift-property rest (string-replace (body-id-value self) #px"[/\\\\]" "-") doc-version doc-date srehto)]
                          [(document-version? self) (sift-property rest doc-id (document-version-text self) doc-date srehto)]
                          [(document-date? self) (sift-property rest doc-id doc-version (document-date-text self) srehto)]
                          [else (sift-property rest doc-id doc-version doc-date (cons self srehto))]))]))))

(define mox-relation-id : (-> Symbol Symbol)
  (lambda [type]
    (gensym type)))
