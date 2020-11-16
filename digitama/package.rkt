#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/symbol)

(require sgml/digitama/document)
(require sgml/digitama/namespace)
(require sgml/digitama/plain/sax)

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [procedure-rename (All (f) (-> f Symbol f))])

(unsafe-require/typed
 file/unzip
 [unzip (->* ((U Input-Port Path-String))
             ((->* (Bytes Boolean Input-Port) ((Option Natural)) Any)
              #:preserve-timestamps? Boolean
              #:utc-timestamps? Boolean)
             Void)])

(define-type MOX-StdIn (U String Path Bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-File-Properties (HashTable Symbol String))

(struct mox-content-types
  ([xmlns : String]
   [extensions : (HashTable PRegexp Symbol)]
   [parts : (HashTable Bytes Symbol)])
  #:type-name MOX-Content-Types
  #:transparent)

(struct mox-relationship
  ([id : Symbol]
   [target : Bytes]
   [type : String]
   [external? : Boolean])
  #:type-name MOX-Relationship
  #:transparent)

(struct mox-relationships
  ([xmlns : String]
   [rels : (HashTable Symbol MOX-Relationship)])
  #:type-name MOX-Relationships
  #:transparent)

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

(struct mox-package-head
  ([types : MOX-Content-Types]
   [rels : MOX-Relationships]
   [part-rels : (HashTable Bytes MOX-Relationships)]
   [properties : MOX-File-Properties]
   [custom : (Option MOX-Custom-Properties)])
  #:type-name MOX-Package-Head
  #:transparent)

(struct mox-package mox-package-head
  ([parts : (HashTable Bytes (U XML-Document (-> Input-Port)))])
  #:type-name MOX-Package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-input-package : (->* (MOX-StdIn) (Symbol) MOX-Package)
  (lambda [/dev/stdin [ooxml 'xlsx]]
    (define documents : (HashTable Bytes (U XML-Document (-> Input-Port))) (make-hash))
    
    (define &types-xmlns : (Boxof String) (box ""))
    (define extensions : (HashTable PRegexp Symbol) (make-hash))
    (define parts : (HashTable Bytes Symbol) (make-hash))

    (define &rels-xmlns : (Boxof String) (box ""))
    (define relationships : (HashTable Symbol MOX-Relationship) (make-hasheq))
    (define part-relationships : (HashTable Bytes MOX-Relationships) (make-hash))

    (define &cprops-xmlns : (Boxof (Option String)) (box #false))
    (define file-properties : MOX-File-Properties (make-hasheq))
    (define custom-properties : (HashTable Symbol MOX-Custom-Property) (make-hasheq))
    
    (define /dev/zipin : Input-Port
      (cond [(input-port? /dev/stdin) /dev/stdin]
            [(bytes? /dev/stdin) (open-input-file (bytes->path /dev/stdin))]
            [else (open-input-file /dev/stdin)]))

    (unzip /dev/zipin
           (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [timestamp : (Option Natural) #false]] : Any
             ;;; There is no folder in Office Open XML Package
             ;;; The input port must be read here, or `unzip` will keep waiting...
             (with-handlers ([exn? (λ [[e : exn]] (port->bytes /dev/pkgin))])
               (define type : Symbol (mox-part-type entry extensions parts))
               
               (case type
                 [(application/vnd.openxmlformats-package.types+xml)
                  (load-xml-datum /dev/pkgin (make-types-sax-handler &types-xmlns extensions parts))]
                 [(application/vnd.openxmlformats-package.relationships+xml)
                  (cond [(bytes=? entry #"_rels/.rels") (load-xml-datum /dev/pkgin (make-relationships-sax-handler &rels-xmlns relationships))]
                        [else (let ([&xmlns : (Boxof String) (box "")]
                                    [rels : (HashTable Symbol MOX-Relationship) (make-hasheq)]
                                    [pentry : Bytes (regexp-replace* #px"[_.]rels($|[/])" entry #"")])
                                (load-xml-datum /dev/pkgin (make-relationships-sax-handler &xmlns rels))
                                (hash-set! part-relationships pentry (mox-relationships (unbox &xmlns) rels)))])]
                 [(application/vnd.openxmlformats-package.core-properties+xml application/vnd.openxmlformats-officedocument.extended-properties+xml)
                  (load-xml-datum /dev/pkgin (make-file-properties-sax-handler file-properties))]
                 [(application/vnd.openxmlformats-officedocument.custom-properties+xml)
                  (load-xml-datum /dev/pkgin (make-custom-properties-sax-handler &cprops-xmlns custom-properties))]
                 [else (let ([stype (symbol->immutable-string type)])
                         (hash-set! documents entry
                                    (cond [(regexp-match? #px"[+]xml$" stype) (read-xml-document /dev/pkgin)]
                                          [else (let ([ooxml::// (string->symbol (format "~a:///~a" ooxml entry))]
                                                      [raw (port->bytes /dev/pkgin)])
                                                  (procedure-rename (λ [] (open-input-bytes raw ooxml:://)) ooxml:://))])))]))))

    (unless (eq? /dev/zipin /dev/stdin)
      (close-input-port /dev/zipin))
    
    (mox-package (mox-content-types (unbox &types-xmlns) extensions parts)
                 (mox-relationships (unbox &rels-xmlns) relationships)
                 part-relationships
                 file-properties
                 (let ([vt (unbox &cprops-xmlns)])
                   (and (string? vt)
                        (mox-custom-properties vt custom-properties)))
                 documents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-types-sax-handler : (-> (Boxof String) (HashTable PRegexp Symbol) (HashTable Bytes Symbol) XML-Event-Handler)
  (lambda [&xmlns extensions parts]
    ((inst make-xml-event-handler Void)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [_ : Void]] : Void
                 (when (pair? attrs)
                   (case element
                     [(Default)
                      (let ([ct (assq 'ContentType attrs)]
                            [et (assq 'Extension attrs)])
                        (when (and ct et) ; Extensions in Racket are dot-prefixed
                          (let ([ext (pregexp (string-append "[.]" (assert (cdr et) string?) "$"))])
                            (hash-set! extensions ext (string->symbol (assert (cdr ct) string?))))))]
                     [(Override)
                      (let ([ct (assq 'ContentType attrs)]
                            [pn (assq 'PartName attrs)])
                        (when (and ct pn) ; ZIP does not store items with leading '/'
                          (let ([name (string->bytes/utf-8 (assert (cdr pn) string?) #false 1)])
                            (hash-set! parts name (string->symbol (assert (cdr ct) string?))))))]
                     [(Types)
                      (let ([?xmlns (assq 'xmlns attrs)])
                        (when (pair? ?xmlns)
                          (set-box! &xmlns (assert (cdr ?xmlns) string?))))]))))))

(define make-relationships-sax-handler : (-> (Boxof String) (HashTable Symbol MOX-Relationship) XML-Event-Handler)
  (lambda [&xmlns rels]
    ((inst make-xml-event-handler Void)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [_ : Void]] : Void
                 (when (pair? attrs)
                   (case element
                     [(Relationship)
                      (let ([mode (assq 'TargetMode attrs)]
                            [target (assq 'Target attrs)]
                            [type (assq 'Type attrs)]
                            [Id (assq 'Id attrs)])
                        (when (and target type Id)
                          (let ([id (string->symbol (assert (cdr Id) string?))])
                            (hash-set! rels id (mox-relationship id
                                                                 (string->bytes/utf-8 (assert (cdr target) string?))
                                                                 (assert (cdr type) string?)
                                                                 (and mode (equal? (cdr mode) "External")))))))]
                     [(Relationships)
                      (let ([?xmlns (assq 'xmlns attrs)])
                        (when (pair? ?xmlns)
                          (set-box! &xmlns (assert (cdr ?xmlns) string?))))]))))))

(define make-file-properties-sax-handler : (-> MOX-File-Properties XML-Event-Handler)
  (lambda [metainfo]
    ((inst make-xml-event-handler Void)
     #:pcdata (λ [[element : Symbol] [depth : Index] [pcdata : String] [cdata? : Boolean] [_ : Void]] : Void
                (let-values ([(ns name) (xml-qname-split element)])
                  (hash-set! metainfo name pcdata))))))

(define make-custom-properties-sax-handler : (-> (Boxof (Option String)) (HashTable Symbol MOX-Custom-Property) (XML-Event-Handlerof CustomAttributes))
  (lambda [&xmlns properties]
    ((inst make-xml-event-handler CustomAttributes)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [_ : CustomAttributes]] : CustomAttributes
                 (displayln element)
                 (when (pair? attrs)
                   (case element
                     [(Properties)
                      (for ([attr (in-list attrs)])
                        (define-values (ns name) (xml-qname-split (car attr)))
                        
                        (when (eq? ns 'xmlns)
                          (set-box! &xmlns (assert (cdr attr) string?))))]
                     [(Property)
                      (let ([name.value (assq 'name attrs)])
                        (when (pair? name.value)
                          (cons (string->symbol (assert (cdr name.value) string?))
                                (for/hasheq : (Immutable-HashTable Symbol String)
                                  ([attr (in-list attrs)] #:when (not (eq? (car attr) 'name)))
                                  (let ([v (cdr attr)])
                                    (cond [(string? v) (values (car attr) v)]
                                          [else (values (car attr) (unbox v))]))))))])))
     #:pcdata (λ [[element : Symbol] [depth : Index] [pcdata : String] [cdata? : Boolean] [name.attrs : CustomAttributes]] : CustomAttributes
                (displayln name.attrs)
                (when (pair? name.attrs)
                  (let-values ([(name) (car name.attrs)]
                               [(ns type) (xml-qname-split element)])
                    (hash-set! properties name
                               (mox-custom-property name type
                                                    pcdata (cdr name.attrs)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-part-type : (-> Bytes (HashTable PRegexp Symbol) (HashTable Bytes Symbol) Symbol)
  (lambda [entry extensions parts]
    (hash-ref parts entry
              (λ [] (or (for/or : (Option Symbol) ([(px.ext type) (in-hash extensions)])
                          (and (regexp-match? px.ext entry) type))
                        (cond [(bytes=? entry #"[Content_Types].xml") 'application/vnd.openxmlformats-package.types+xml]
                              [else '||]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CustomAttributes (U Void (Pairof Symbol (Immutable-HashTable Symbol String))))
