#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)

(require digimon/archive)
(require sgml/digitama/plain/sax)

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [procedure-rename (All (f) (-> f Symbol f))])

(require "moxml.rkt")
(require "mox/datatype.rkt")
(require "shared/moxml.rkt")
(require "drawing/moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Stdin (U String Path Bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-content-types
  ([xmlns : String]
   [extensions : (HashTable PRegexp Symbol)]
   [parts : (HashTable String Symbol)])
  #:type-name MOX-Content-Types
  #:transparent)

(struct mox-relationship
  ([id : Symbol]
   [target : String]
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
   [part-rels : (HashTable String MOX-Relationships)])
  #:type-name MOX.ZIP)

(struct (x) mox.ml mox.zip
  ([drawing : MOX-DrawingML]
   [shared : MOX-SharedML]
   [document : x])
  #:type-name MOX.ML
  #:transparent)

(struct (x) mox-package mox.ml
  ([orphans : (HashTable String (Pairof Symbol (-> Input-Port)))])
  #:type-name MOX-Packageof)

(struct mox-package-template
  ([orphans : (HashTable String (Pairof Symbol Bytes))])
  #:type-name MOX-Package-Template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (x) mox-input-package-for-template : (-> MOX-Stdin (MOXML-Agentof (∩ MOXML x)) MOX-Package-Template)
  (lambda [/dev/stdin mox-agent]
    (define-values (ooxml mox-unzip mox-realize) (mox-agent))

    (define orphans : (HashTable String (Pairof Symbol Bytes)) (make-hash))
    
    (define &types-xmlns : (Boxof String) (box ""))
    (define extensions : (HashTable PRegexp Symbol) (make-hash))
    (define parts : (HashTable String Symbol) (make-hash))

    (define &rels-xmlns : (Boxof String) (box ""))
    (define relationships : (HashTable Symbol MOX-Relationship) (make-hasheq))
    (define part-relationships : (HashTable String MOX-Relationships) (make-hash))

    (define /dev/zipin : Input-Port
      (cond [(input-port? /dev/stdin) /dev/stdin]
            [(bytes? /dev/stdin) (open-input-file (bytes->path /dev/stdin))]
            [else (open-input-file /dev/stdin)]))

    (zip-extract /dev/zipin
                 ;;; NOTE that MS Office Open XML Package dosen't keep folders in archive.
                 (λ [[/dev/pkgin : Input-Port] [entry : String] [folder? : Boolean] [timestamp : Natural] [datum : Any]] : Any
                   (define type : Symbol (mox-part-type entry extensions parts))
                   
                   (or (mox-unzip entry type /dev/pkgin)
                       
                       (case type
                         [(application/vnd.openxmlformats-package.types+xml)
                          (load-xml-datum /dev/pkgin (make-types-sax-handler &types-xmlns extensions parts))]
                         [(application/vnd.openxmlformats-package.relationships+xml)
                          (cond [(string=? entry "_rels/.rels") (load-xml-datum /dev/pkgin (make-relationships-sax-handler &rels-xmlns relationships))]
                                [else (let ([&xmlns : (Boxof String) (box "")]
                                            [rels : (HashTable Symbol MOX-Relationship) (make-hasheq)]
                                            [pentry : String (regexp-replace* #px"[_.]rels($|[/])" entry "")])
                                        (load-xml-datum /dev/pkgin (make-relationships-sax-handler &xmlns rels))
                                        (hash-set! part-relationships pentry (mox-relationships (unbox &xmlns) rels)))])]
                         [else (hash-set! orphans entry (cons type (port->bytes /dev/pkgin)))]))))

    (unless (eq? /dev/zipin /dev/stdin)
      (close-input-port /dev/zipin))
    
    (mox-package-template orphans)))

(define #:forall (x) mox-input-package : (-> MOX-Stdin (MOXML-Agentof (∩ MOXML x)) (MOX-Packageof x))
  (lambda [/dev/stdin mox-agent]
    (define-values (_s shared-unzip shared-realize) (moxml-sharedml-agent))
    (define-values (_d drawing-unzip drawing-realize) (moxml-drawingml-agent))
    (define-values (ooxml mox-unzip mox-realize) (mox-agent))

    (define orphans : (HashTable String (Pairof Symbol (-> Input-Port))) (make-hash))
    
    (define &types-xmlns : (Boxof String) (box ""))
    (define extensions : (HashTable PRegexp Symbol) (make-hash))
    (define parts : (HashTable String Symbol) (make-hash))

    (define &rels-xmlns : (Boxof String) (box ""))
    (define relationships : (HashTable Symbol MOX-Relationship) (make-hasheq))
    (define part-relationships : (HashTable String MOX-Relationships) (make-hash))

    (define /dev/zipin : Input-Port
      (cond [(input-port? /dev/stdin) /dev/stdin]
            [(bytes? /dev/stdin) (open-input-file (bytes->path /dev/stdin))]
            [else (open-input-file /dev/stdin)]))

    (zip-extract /dev/zipin
                 ;;; NOTE that MS Office Open XML Package dosen't keep folders in archive.
                 (λ [[/dev/pkgin : Input-Port] [entry : String] [folder? : Boolean] [timestamp : Natural] [datum : Any]] : Any
                   (define type : Symbol (mox-part-type entry extensions parts))

                   (or (mox-unzip entry type /dev/pkgin)
                       (drawing-unzip entry type /dev/pkgin)
                       (shared-unzip entry type /dev/pkgin)

                       (case type
                         [(application/vnd.openxmlformats-package.types+xml)
                          (load-xml-datum /dev/pkgin (make-types-sax-handler &types-xmlns extensions parts))]
                         [(application/vnd.openxmlformats-package.relationships+xml)
                          (cond [(string=? entry "_rels/.rels") (load-xml-datum /dev/pkgin (make-relationships-sax-handler &rels-xmlns relationships))]
                                [else (let ([&xmlns : (Boxof String) (box "")]
                                            [rels : (HashTable Symbol MOX-Relationship) (make-hasheq)]
                                            [pentry : String (regexp-replace* #px"[_.]rels($|[/])" entry "")])
                                        (load-xml-datum /dev/pkgin (make-relationships-sax-handler &xmlns rels))
                                        (hash-set! part-relationships pentry (mox-relationships (unbox &xmlns) rels)))])]
                         [else ; might use them later
                          (hash-set! orphans entry
                                     (let ([ooxml::// (format "~a:///~a" ooxml entry)]
                                           [immediately-extracted-raw (port->bytes /dev/pkgin)])
                                       (cons type (procedure-rename (λ [] (open-input-bytes immediately-extracted-raw ooxml:://))
                                                                    (string->symbol ooxml:://)))))]))))

    (unless (eq? /dev/zipin /dev/stdin)
      (close-input-port /dev/zipin))
    
    (mox-package (mox-content-types (unbox &types-xmlns) extensions parts)
                 (mox-relationships (unbox &rels-xmlns) relationships)
                 part-relationships
                 (drawing-realize)
                 (shared-realize)
                 (mox-realize)
                 orphans)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-types-sax-handler : (-> (Boxof String) (HashTable PRegexp Symbol) (HashTable String Symbol) XML-Event-Handler)
  ;;; OpenPackagingConventions 9.1.2.2
  ; For file readers, if an element matches both Default and Override, Override takes precedence. 
  (lambda [&xmlns extensions parts]
    ((inst make-xml-event-handler Void)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [?preserve : Boolean] [_ : Void]] : Void
                 (when (pair? attrs)
                   (case element
                     [(Default)
                      (let ([ct (assq 'ContentType attrs)]
                            [et (assq 'Extension attrs)])
                        (when (and ct et) ; Extensions in Racket are dot-prefixed
                          (let ([ext (pregexp (string-append "[.]" (xml:attr-value->string (cdr et)) "$"))])
                            (hash-set! extensions ext (xml:attr-value->symbol (cdr ct))))))]
                     [(Override)
                      (let ([ct (assq 'ContentType attrs)]
                            [pn (assq 'PartName attrs)])
                        (when (and ct pn)
                          (hash-set! parts
                                     (substring (xml:attr-value->string (cdr pn)) 1)  ; ZIP does not store items with leading '/'
                                     (xml:attr-value->symbol (cdr ct)))))]
                     [(Types)
                      (let ([?xmlns (assq 'xmlns attrs)])
                        (when (pair? ?xmlns)
                          (set-box! &xmlns (xml:attr-value->string (cdr ?xmlns)))))]))))))

(define make-relationships-sax-handler : (-> (Boxof String) (HashTable Symbol MOX-Relationship) XML-Event-Handler)
  (lambda [&xmlns rels]
    ((inst make-xml-event-handler Void)
     #:element (λ [[element : Symbol] [depth : Index] [attrs : (Option SAX-Attributes)] [?empty : Boolean] [?preserve : Boolean] [_ : Void]] : Void
                 (when (pair? attrs)
                   (case element
                     [(Relationship)
                      (let ([mode (assq 'TargetMode attrs)]
                            [target (assq 'Target attrs)]
                            [type (assq 'Type attrs)]
                            [Id (assq 'Id attrs)])
                        (when (and target type Id)
                          (let ([id (mox:attr-value->relationship-id (cdr Id))])
                            (hash-set! rels id (mox-relationship id
                                                                 (xml:attr-value->string (cdr target))
                                                                 (xml:attr-value->symbol (cdr type))
                                                                 (and mode (equal? (cdr mode) "External")))))))]
                     [(Relationships)
                      (let ([?xmlns (assq 'xmlns attrs)])
                        (when (pair? ?xmlns)
                          (set-box! &xmlns (xml:attr-value->string (cdr ?xmlns)))))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-part-type : (-> String (HashTable PRegexp Symbol) (HashTable String Symbol) Symbol)
  (lambda [entry extensions parts]
    (hash-ref parts entry
              (λ [] (or (for/or : (Option Symbol) ([(px.ext type) (in-hash extensions)])
                          (and (regexp-match? px.ext entry) type))
                        (cond [(string=? entry "[Content_Types].xml") 'application/vnd.openxmlformats-package.types+xml]
                              [else '||]))))))
