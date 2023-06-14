#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/namespace)
(require sgml/digitama/plain/datatype)
(require sgml/digitama/plain/sax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-File-Properties (HashTable Symbol String))

(struct mox-custom-property
  ([name : Symbol]
   [type : Symbol]
   [value : String]
   [attributes : (Immutable-HashTable Symbol String)])
  #:type-name MOX-Custom-Property
  #:transparent)

(struct mox-custom-properties
  ([xmlns : String]
   [cprops : (HashTable Symbol MOX-Custom-Property)])
  #:type-name MOX-Custom-Properties
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-file-properties-sax-handler : (-> MOX-File-Properties XML-Event-Handler)
  (lambda [metainfo]
    ((inst make-xml-event-handler Void)
     #:pcdata (λ [[element : Symbol] [depth : Index] [pcdata : String] [preserve? : Boolean] [cdata? : Boolean] [_ : Void]] : Void
                (let-values ([(ns name) (xml-qname-split element)])
                  (hash-set! metainfo name pcdata))))))

(define make-custom-properties-sax-handler : (-> (Boxof (Option String)) (HashTable Symbol MOX-Custom-Property) (XML-Event-Handlerof CustomAttributes))
  (lambda [&xmlns properties]
    ((inst make-xml-event-handler CustomAttributes)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [?preserve : Boolean] [_ : CustomAttributes]]
                 : CustomAttributes
                 (when (pair? attrs)
                   (case element
                     [(Properties)
                      (for ([attr (in-list attrs)])
                        (define-values (ns name) (xml-qname-split (car attr)))
                        
                        (when (eq? ns 'xmlns)
                          (set-box! &xmlns (xml:attr-value->string (cdr attr)))))]
                     [(Property)
                      (let ([name.value (assq 'name attrs)])
                        (when (pair? name.value)
                          (cons (string->symbol (assert (cdr name.value) string?))
                                (for/hasheq : (Immutable-HashTable Symbol String)
                                  ([attr (in-list attrs)] #:when (not (eq? (car attr) 'name)))
                                  (let ([v (cdr attr)])
                                    (cond [(string? v) (values (car attr) v)]
                                          [else (values (car attr) (unbox v))]))))))])))
     #:pcdata (λ [[element : Symbol] [depth : Index] [pcdata : String] [preserve? : Boolean] [cdata? : Boolean] [name.attrs : CustomAttributes]] : CustomAttributes
                (when (pair? name.attrs)
                  (let-values ([(name) (car name.attrs)]
                               [(ns type) (xml-qname-split element)])
                    (hash-set! properties name
                               (mox-custom-property name type
                                                    pcdata (cdr name.attrs)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CustomAttributes (U Void (Pairof Symbol (Immutable-HashTable Symbol String))))
