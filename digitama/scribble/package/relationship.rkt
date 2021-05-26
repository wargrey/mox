#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require racket/symbol)
(require racket/path)

(require "partname.rkt")
(require "standards.rkt")

(require "../shared/render.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type OPC-Relationship (List Symbol String String Boolean))

(define opc-make-internal-relationship : (case-> [(Pairof String Symbol) -> OPC-Relationship]
                                                 [Symbol String (U String Symbol) -> OPC-Relationship])
  (case-lambda
    [(override)
     (opc-make-internal-relationship (mox-relation-id (cdr override)) (car override) (cdr override))]
    [(id target type)
     (list id target (opc-relationship-type type) #false)]))

(define opc-make-external-relationship : (case-> [(Pairof String Symbol) -> OPC-Relationship]
                                                 [Symbol String (U String Symbol) -> OPC-Relationship])
  (case-lambda
    [(override)
     (opc-make-external-relationship (mox-relation-id (cdr override)) (car override) (cdr override))]
    [(id target type)
     (list id target (opc-relationship-type type) #true)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-relationships-markup-entry : (->* (String) ((Listof OPC-Relationship) #:utc Integer) Archive-Entry) 
  (lambda [base [elements null] #:utc [ts #false]]
    (define partname : String (opc-relationship-part-name base))
    (define entry-name : String (opc-part-name-normalize/zip base))
    
    (define relationships.xml : Xexpr
      (list 'Relationships `([xmlns . ,(assert (opc-xmlns 'Relationships))])
            (for/list : (Listof Xexpr) ([e (in-list elements)])
              (opc-relation-element->relationship entry-name e))))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 8.3.3.1, 2006"
                              (xexpr->bytes relationships.xml #:prolog? #true)
                              (opc-part-name-normalize/zip partname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-relationship-part-name : (-> String String)
  (lambda [base]
    (define basename : (Option Path) (file-name-from-path base))

    (cond [(not basename) (path->string (build-path base "_rels/.rels"))]
          [else (path->string (build-path (assert (path-only base)) "_rels" (path-add-extension basename ".rels" ".")))])))

(define opc-relationship-relative-entry-name : (-> String String String)
  (lambda [base target]
    (cond [(string=? base "") target]
          [else (let ([dirname (path-only (string->some-system-path base 'unix))])
                  (cond [(not dirname) target]
                        [else (let ([unix-target (string->some-system-path target 'unix)])
                                (some-system-path->string (find-relative-path dirname unix-target)))]))])))

(define opc-relation-element->relationship : (-> String OPC-Relationship Xexpr)
  (lambda [entry-name elem]
    (define target-entry : String (opc-part-name-normalize/zip (cadr elem)))
    (define external? : Boolean (cadddr elem))
    
    (define attlist : (Listof (Pairof Symbol String))
      `([Id         . ,(symbol->immutable-string (car elem))]
        [Target     . ,(if (not external?) (opc-relationship-relative-entry-name entry-name target-entry) target-entry)]
        [Type       . ,(caddr elem)]))
    
    (list 'Relationship (cond [(not external?) attlist]
                              [else (cons (cons 'TargetMode "External")
                                          attlist)]))))
