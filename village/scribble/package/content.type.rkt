#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require racket/symbol)
(require racket/list)

(require "partname.rkt")
(require "standards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-content-types-markup-entry : (->* () ((Listof (Pairof String Symbol)) (Listof Symbol) #:utc Integer) Archive-Entry)
  (lambda [[overrides null] [defaults null] #:utc [ts #false]]
    (define content-type.xml : XExpr
      (list 'Types `([xmlns . ,(assert (opc-xmlns 'Types))])
            (append (filter-map opc-type->default (remove-duplicates (list* 'xml 'rels defaults)))
                    (filter-map opc-type->override ((inst remove-duplicates (Pairof String Symbol) String) overrides string=? #:key car)))))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 9.1.2.2, 2006"
                              (xexpr->bytes content-type.xml #:prolog? #true)
                              (opc-part-name-normalize/zip "/[Content_Types].xml"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-type->default : (-> Symbol (Option XExpr))
  (lambda [ext]
    (define content-type : (Option Symbol) (opc-type-name ext))

    (and content-type
         `(Default ([Extension . ,(symbol->immutable-string ext)]
                    [ContentType . ,(symbol->immutable-string content-type)])))))

(define opc-type->override : (-> (Pairof String Symbol) (Option XExpr))
  (lambda [t]
    (define content-type : (Option Symbol) (opc-override-type-name (cdr t)))
    
    (and content-type
         `(Override ([PartName . ,(opc-part-name-normalize (car t))]
                     [ContentType . ,(symbol->immutable-string content-type)])))))
