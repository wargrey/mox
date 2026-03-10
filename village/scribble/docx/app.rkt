#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)
(require digimon/digitama/collection)

(require typed/setup/getinfo)
(require syntax/location)

(require sgml/xexpr)

(require "../package/core.rkt")
(require "../package/partname.rkt")
(require "../package/standards.rkt")
(require "../shared/dtrace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-properties-markup-entries : (->* (String (Option String) MOX-NameList String String (Listof Any))
                                                  (#:utc Integer)
                                                  (Listof (Pairof Symbol Archive-Entry)))
  (lambda [part-name-fmt ?title authors doc-version timestamp properties #:utc [ts #false]]
    (define metainfo : MOX-Metainfo (mox-properties-metainfo-ref properties))
    (define info : Pkg-Info (assert (single-collection-info (quote-source-file))))
    (define info-ref : Info-Ref (pkg-info-ref info))

    (define core-properties : Archive-Entry
      (mox-core-properties-markup-entries part-name-fmt metainfo (or ?title "") authors doc-version timestamp #:utc ts))
    
    (define wordProperty.xml : XExpr
      (list 'Properties `([xmlns . ,(assert (opc-xmlns 'App))]
                          [xmlns:vt . ,(assert (opc-xmlns 'App:VT))])
            (mox-shared-application-properties-xexprs metainfo
                                                      "devimon (https://github.com/wargrey/mox) ~a"
                                                      (info-ref 'version (λ [] 1.0)))))

    (list (cons 'Core core-properties)
          (cons 'App (make-archive-ascii-entry #:utc-time ts #:comment "Fundamentals 15.2.11.3, 2006"
                                               (xexpr->bytes wordProperty.xml #:prolog? #true)
                                               (opc-part-name-normalize/zip (format part-name-fmt "app.xml")))))))
