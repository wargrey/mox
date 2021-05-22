#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require racket/symbol)

(require "partname.rkt")
(require "standards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type OPC-Relationship (List Symbol String String Boolean))

(define opc-make-internal-relationship : (-> Symbol String (U String Symbol) OPC-Relationship)
  (lambda [id target type]
    (list id target (opc-relationship-type type) #false)))

(define opc-make-external-relationship : (-> Symbol String (U String Symbol) OPC-Relationship)
  (lambda [id target type]
    (list id target (opc-relationship-type type) #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-relationships-markup-entry : (->* (String) ((Listof OPC-Relationship) #:utc Integer) Archive-Entry) 
  (lambda [partname [elements null] #:utc [ts #false]]
    (define relationships.xml : Xexpr
      (list 'Relationships `([xmlns . ,(assert (opc-xmlns 'Relationships))])
            (map opc-relation-element->relationship elements)))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 8.3.3.1, 2006"
                              (xexpr->bytes relationships.xml #:prolog? #true)
                              (opc-part-name-normalize/zip partname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-relation-element->relationship : (-> OPC-Relationship Xexpr)
  (lambda [elem]
    (define attlist : (Listof (Pairof Symbol String))
      `([Id         . ,(symbol->immutable-string (car elem))]
        [Target     . ,(opc-part-name-normalize/zip (cadr elem))]
        [Type       . ,(caddr elem)]))
    
    (list 'Relationship (cond [(cadddr elem) (cons (cons 'TargetMode "External") attlist)]
                              [else attlist]))))
