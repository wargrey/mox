#lang typed/racket/base

(provide (all-defined-out) XML-Attribute-Extract)
(provide (all-from-out sgml/digitama/xexpr/dialect))

(require racket/symbol)

(require digimon/syntax)
(require digimon/struct)

(require sgml/digitama/xexpr/dialect)
(require sgml/digitama/xexpr/grammar)

(require (for-syntax syntax/parse))
(require (for-syntax racket/string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->mox:elem-names <prefix> <e>)
  (define prefix (symbol->immutable-string (syntax-e <prefix>)))
  (define elem (symbol->immutable-string (syntax-e <e>)))
  (list (format-id <e> "~a:~a" prefix elem)
        (format-id <e> "~a:~a" (string-upcase prefix) (string-titlecase elem))))

(define-for-syntax (racket->mox:attr-names <prefix> <a>)
  (define prefix (symbol->immutable-string (syntax-e <prefix>)))
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (list (format-id <a> "~a#~a" prefix attrib)
        (format-id <a> "~a#~a" (string-upcase prefix) (string-titlecase attrib))
        (format-id <a> "extract-~a#~a" prefix attrib)
        (format-id <a> "~a#~a->xml-attributes" prefix attrib)))

(define-for-syntax (has-mandatory-attribute? <attr-defs>)
  (define attr-defs (syntax-e <attr-defs>))
  (for/or ([<def> (in-list (syntax-e <attr-defs>))])
    (define maybe-default (memq '#:= (syntax->datum <def>)))
    (or (not maybe-default)
        (null? (cdr maybe-default))
        (null? (cadr maybe-default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-mox-element stx)
  (syntax-parse stx #:literals [:]
    [(_ (~optional (~seq (~and #:root kw-root))) id #:for x
        (~optional (~seq #:-> parent) #:defaults ([parent #'mox-attribute]))
        (~optional (~seq (~and #:attlist kw-attlist) (attr-defs ...))
                   #:defaults ([(attr-defs 1) null]))
        (field-defs ...))
     (with-syntax* ([(elem Elem) (racket->mox:elem-names #'x #'id)]
                    [(src) (list (format-id #'id "src"))]
                    [(xmlns attlist) (list (format-id #'id "xmlns") (format-id #'id "attlist"))]
                    [(mox:attr MOX:Attr extract-attr attr->xexpr) (racket->mox:attr-names #'x #'id)]
                    [(AttlistType defattr ...) (if (has-mandatory-attribute? #'(attr-defs ...)) (list #'MOX:Attr) (list #'(Option MOX:Attr) #'#false))]
                    [(defs-for-root ...) (if (attribute kw-root) (list #'[xmlns : XML-Namespaces null]) null)]
                    [(defs-for-attr ...) (if (null? (syntax-e #'[attr-defs ...])) null (list #'[attlist : AttlistType defattr ...]))]
                    [define-attr (if (attribute kw-attlist) #'(define-mox-attribute id #:for x #:-> parent (attr-defs ...)) #'(void))])
       (syntax/loc stx
         (begin define-attr
                (define-struct elem : Elem ([src : Any #false] defs-for-root ... defs-for-attr ... field-defs ...) #:transparent))))]))

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
