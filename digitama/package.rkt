#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/symbol)

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

(require "moxml.rkt")
(require "shared/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-StdIn (U String Path Bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-content-types
  ([xmlns : String]
   [extensions : (HashTable PRegexp Symbol)]
   [parts : (HashTable Bytes Symbol)])
  #:type-name MOX-Content-Types
  #:transparent)

(struct mox-relationship
  ([id : Symbol]
   [target : Bytes]
   [type : Symbol]
   [external? : Boolean])
  #:type-name MOX-Relationship
  #:transparent)

(struct mox-relationships
  ([xmlns : String]
   [rels : (HashTable Symbol MOX-Relationship)])
  #:type-name MOX-Relationships
  #:transparent)

(struct mox.zip
  ([types : MOX-Content-Types]
   [rels : MOX-Relationships]
   [part-rels : (HashTable Bytes MOX-Relationships)])
  #:type-name MOX.ZIP)

(struct mox.ml mox.zip
  ([shared : MOX-SharedML]
   [document : MOXML])
  #:type-name MOX.ML
  #:transparent)

(struct mox-package mox.ml
  ([orphans : (HashTable Bytes (Pairof Symbol (-> Input-Port)))])
  #:type-name MOX-Package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (x) mox-input-package : (-> MOX-StdIn (MOXML-Agentof (∩ MOXML x)) MOX-Package)
  (lambda [/dev/stdin mox-agent]
    (define-values (_ shared-unzip shared-realize) (moxml-sharedml-agent))
    (define-values (ooxml mox-unzip mox-realize) (mox-agent))

    (define orphans : (HashTable Bytes (Pairof Symbol (-> Input-Port))) (make-hash))
    
    (define &types-xmlns : (Boxof String) (box ""))
    (define extensions : (HashTable PRegexp Symbol) (make-hash))
    (define parts : (HashTable Bytes Symbol) (make-hash))

    (define &rels-xmlns : (Boxof String) (box ""))
    (define relationships : (HashTable Symbol MOX-Relationship) (make-hasheq))
    (define part-relationships : (HashTable Bytes MOX-Relationships) (make-hash))

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
               
               (or (mox-unzip entry type /dev/pkgin)
                   (shared-unzip entry type /dev/pkgin)

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
                     [else (let ([stype (symbol->immutable-string type)])
                             (hash-set! orphans entry
                                        (let ([ooxml::// (format "~a:///~a" ooxml entry)]
                                              [raw (port->bytes /dev/pkgin)])
                                          (cons type
                                                (procedure-rename (λ [] (open-input-bytes raw ooxml:://))
                                                                  (string->symbol ooxml:://))))))])))))

    (unless (eq? /dev/zipin /dev/stdin)
      (close-input-port /dev/zipin))
    
    (mox-package (mox-content-types (unbox &types-xmlns) extensions parts)
                 (mox-relationships (unbox &rels-xmlns) relationships)
                 part-relationships
                 (shared-realize)
                 (mox-realize)
                 orphans)))

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
                                                                 (string->symbol (assert (cdr type) string?))
                                                                 (and mode (equal? (cdr mode) "External")))))))]
                     [(Relationships)
                      (let ([?xmlns (assq 'xmlns attrs)])
                        (when (pair? ?xmlns)
                          (set-box! &xmlns (assert (cdr ?xmlns) string?))))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-part-type : (-> Bytes (HashTable PRegexp Symbol) (HashTable Bytes Symbol) Symbol)
  (lambda [entry extensions parts]
    (hash-ref parts entry
              (λ [] (or (for/or : (Option Symbol) ([(px.ext type) (in-hash extensions)])
                          (and (regexp-match? px.ext entry) type))
                        (cond [(bytes=? entry #"[Content_Types].xml") 'application/vnd.openxmlformats-package.types+xml]
                              [else '||]))))))
