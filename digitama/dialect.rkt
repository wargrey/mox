#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out sgml/digitama/plain/dialect))

(require racket/symbol)
(require digimon/syntax)

(require sgml/digitama/plain/dialect)
(require sgml/digitama/plain/grammar)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->mox:attr-names <prefix> <a>)
  (define prefix (symbol->immutable-string (syntax-e <prefix>)))
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (list (format-id <a> "~a:attr:~a" prefix attrib)
        (format-id <a> "~a:Attr:~a" (string-upcase prefix) (string-titlecase attrib))
        (format-id <a> "extract-~a:attr:~a" prefix attrib)
        (format-id <a> "~a:attr:~a->xml-attributes" prefix attrib)))

(define-syntax (define-mox-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ attr-name #:for x (~optional (~seq #:-> parent) #:defaults ([parent #'mox-attribute]))
        ([field : FieldType (~optional (~seq #:= default-vals) #:defaults ([default-vals #'[]])) def-rest ...] ...)
        options ...)
     (with-syntax* ([(mox:attr MOX:Attr extract-attr attr->xexpr) (racket->mox:attr-names #'x #'attr-name)]
                    [(attr-ref ...) (make-identifiers #'mox:attr #'(field ...))]
                    [(attr-ref* ...) (make-identifiers #'mox:attr #'(field ...) "~a-~a*")]
                    [([OptionType DefinitionType [absent-val ...] [default-val ...]] ...)
                     (for/list ([<type> (in-list (syntax->list #'(FieldType ...)))]
                                [<field> (in-list (syntax->list #'(field ...)))]
                                [<defv> (in-list (syntax->list #'(default-vals ...)))])
                       (define defvals (syntax-e <defv>))
                       (cond [(not (list? defvals)) (list <defv> <defv> (list <defv>) null)]
                             [(null? defvals) (list <type> <type> null null)]
                             [(null? (cdr defvals)) (list (car defvals) (car defvals) (list (car defvals)) null)]
                             [else (list (car defvals) <type> (list (car defvals)) (cdr defvals))]))])
       (syntax/loc stx
         (begin (define-xml-attribute mox:attr : MOX:Attr #:-> parent #:with extract-attr attr->xexpr
                  ([field : (U FieldType OptionType) #:= absent-val ... def-rest ...] ...)
                  #:report mox-report-unrecognized-attributes #false
                  options ...)
                
                (define attr-ref* : (-> MOX:Attr (U FieldType DefinitionType))
                  (lambda [self]
                    (or (attr-ref self) default-val ...)))
                ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-attribute () #:type-name MOX-Attribute #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-report-unrecognized-attributes : (-> (Option Symbol) (Listof XML-Element-Attribute) Void)
  (lambda [elem srtta]
    (define /dev/stderr : Output-Port (current-error-port))
    (define logstem : String "unrecognized attribute: ~a = ~s")
    (define logfmt : String (if (not elem) logstem (string-append (symbol->immutable-string elem) ": " logstem)))
    (for ([attr (in-list (reverse srtta))])
      (fprintf /dev/stderr logfmt (car attr) (cdr attr)))))
