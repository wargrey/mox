#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)

(require sgml/digitama/document)
(require sgml/digitama/plain/grammar)

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
(struct mox-content-type
  ([xmlns : String]
   [extensions : (HashTable Bytes String)]
   [parts : (HashTable Bytes String)])
  #:type-name MOX-Content-Type
  #:transparent)

(struct mox-package-head
  ([types : MOX-Content-Type])
  #:type-name MOX-Package-Head
  #:transparent)

(struct mox-package mox-package-head
  ([parts : (HashTable Bytes (U XML-Document (-> Input-Port)))])
  #:type-name MOX-Package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-input-package : (->* (MOX-StdIn) (Symbol) MOX-Package)
  (lambda [/dev/stdin [ooxml 'xlsx]]
    (define documents : (HashTable Bytes (U XML-Document (-> Input-Port))) (make-hash))
    (define &type-xmlns : (Boxof String) (box ""))
    (define extensions : (HashTable Bytes String) (make-hash))
    (define parts : (HashTable Bytes String) (make-hash))
    
    (define /dev/zipin : Input-Port
      (cond [(input-port? /dev/stdin) /dev/stdin]
            [(bytes? /dev/stdin) (open-input-file (bytes->path /dev/stdin))]
            [else (open-input-file /dev/stdin)]))

    (unzip /dev/zipin
           (λ [[entry : Bytes] [dir? : Boolean] [/dev/xlsxin : Input-Port] [timestamp : (Option Natural) #false]] : Any
             ;;; There is no folder in Office Open XML Package
             ;;; The input port must be read here, or `unzip` will keep waiting...
             (displayln entry)
             (with-handlers ([exn? (λ [[e : exn]] (port->bytes /dev/xlsxin))])
               (define content : (U XML-Document (-> Input-Port))
                 (cond [(regexp-match? #px"[.][Xx][Mm][Ll]$" entry) (read-xml-document /dev/xlsxin)]
                                [else (let ([ooxml::// (string->symbol (format "~a:///~a" ooxml entry))]
                                            [raw (port->bytes /dev/xlsxin)])
                                        (procedure-rename (λ [] (open-input-bytes raw ooxml:://)) ooxml:://))]))


               (cond [(bytes=? entry #"[Content_Types].xml") (and (xml-document? content) (xml-extract-content-types content &type-xmlns extensions parts))]
                     [else (hash-set! documents entry content)]))))

    (unless (eq? /dev/zipin /dev/stdin)
      (close-input-port /dev/zipin))
    
    (mox-package (mox-content-type (unbox &type-xmlns) extensions parts)
                 documents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-extract-content-types : (-> XML-Document (Boxof String) (HashTable Bytes String) (HashTable Bytes String) Void)
  (lambda [types.xml &xmlns extensions parts]
    (for ([types (in-list (xml-document-elements types.xml))])
      (when (and (list? types) (eq? (car types) 'Types))
        (define ?xmlns (assq 'xmlns (cadr types)))
        (when (pair? ?xmlns) (set-box! &xmlns (assert (cdr ?xmlns) string?)))

        (for ([type (in-list (caddr types))])
          (when (list? type)
            (define attrs : (Listof XML-Element-Attribute) (cadr type))
            (define ct (assq 'ContentType attrs))

            (unless (not ct)
              (define content-type : String (assert (cdr ct) string?))
              (define ext (assq 'Extension attrs))
              (define pn (assq 'PartName attrs))

              (when (pair? ext) ; Racket extension is dot-prefixed
                (hash-set! extensions (string->bytes/utf-8 (string-append "." (assert (cdr ext) string?))) content-type))
              
              (when (pair? pn) ; ZIP does not store items with leading '/'
                (hash-set! parts (string->bytes/utf-8 (substring (assert (cdr pn) string?) 1)) content-type)))))))))
