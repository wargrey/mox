#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)
(require digimon/iana)

(require sgml/xml)

(require racket/symbol)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-content-types-stream-markup-entry : (->* () ((Listof Symbol) (Listof (Pairof String Symbol)) #:utc Integer) Archive-Entry)
  ;;; OpenPackagingConventions 9.1.2.2 
  (lambda [[defaults null] [overrides null] #:utc [ts #false]]
    (define content-type : Xexpr
      (list 'Types '([xmlns . "http://schemas.openxmlformats.org/package/2006/content-types"])
            (append (filter-map opc-type->default (remove-duplicates (list* 'xml 'rels defaults)))
                    (filter-map opc-type->override ((inst remove-duplicates (Pairof String Symbol) String) overrides string=? #:key car)))))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 9.1.2.2, 2006"
                              (xexpr->bytes content-type #:prolog? #true) "[Content_Types].xml")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-type->default : (-> Symbol (Option Xexpr))
  (lambda [ext]
    (define content-type : (Option Symbol) (opc-type-name ext))

    (and content-type
         `(Default ([Extension . ,(symbol->immutable-string ext)]
                    [ContentType . ,(symbol->immutable-string content-type)])))))

(define opc-type->override : (-> (Pairof String Symbol) (Option Xexpr))
  (lambda [t]
    (define content-type : (Option Symbol) (opc-type-name (cdr t)))
    
    (and content-type
         `(Override ([PartName . ,(opc-part-name-normalize (car t))]
                     [ContentType . ,(symbol->immutable-string content-type)])))))

(define opc-type-name : (-> Symbol (Option Symbol))
  (lambda [ext]
    (case ext
      [(rels) 'application/vnd.openxmlformats-package.relationships+xml]
      [else (iana-media-type-ref ext)])))

(define opc-part-name-normalize : (-> String String)
  (lambda [name]
    ; MOX part names are prefixed with '/', which should be removed for zip entries
    (substring name 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(opc-content-types-stream-markup-entry)
