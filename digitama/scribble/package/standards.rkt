#lang typed/racket/base

;;; OpenPackagingConventions - Standard Namespaces and Content Types

(provide (all-defined-out))

(require digimon/iana)

(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-xmlns : (-> Symbol (Option String))
  (lambda [part]
    (case part
      [(Types)         "http://schemas.openxmlformats.org/package/2006/content-types"]
      [(App)           "http://schemas.openxmlformats.org/package/2006/extended-properties"]
      [(App:VT)        "http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"]
      [(Core:CP)       "http://schemas.openxmlformats.org/package/2006/core-properties"]
      [(Core:DCTerms)  "http://purl.org/dc/terms/"]
      [(Core:DCMIType) "http://purl.org/dc/dcmityp/"]
      [(Core:DC)       "http://purl.org/dc/elements/1.1/"]
      [(Core:XSI)      "http://www.w3.org/2001/XMLSchema-instance"]
      [(Signatures)    "http://schemas.openxmlformats.org/package/2006/digital-signature"]
      [(Relationships) "http://schemas.openxmlformats.org/package/2006/relationships"]
      [(Compatibility) "http://schemas.openxmlformats.org/markup-compatibility/2006"]
      [(document.xml)  "http://schemas.openxmlformats.org/wordprocessingml/2006/main"]
      [else #false])))

(define opc-relationship-type : (-> (U String Symbol) String)
  (lambda [type]
    (cond [(string? type) type]
          [else (case type ; should be consistent with those in `opc-override-type-name`
                  [(App)          "http://schemas.openxmlformats.org/package/2006/relationships/extended-properties"]
                  [(Core)         "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"]
                  [(Thumbnail)    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail"]
                  [(Certificate)  "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/certificate"]
                  [(Signature)    "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin"]
                  [(document.xml) "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"]
                  [else (symbol->immutable-string type)])])))

(define opc-type-name : (-> Symbol (Option Symbol))
  (lambda [ext]
    (case ext
      [(rels) 'application/vnd.openxmlformats-package.relationships+xml]
      [(core) 'application/vnd.openxmlformats-package.core-properties+xml]
      [(cert) 'application/vnd.openxmlformats-package.digital-signature-certificate]
      [(osig) 'application/vnd.openxmlformats-package.digital-signature-origin]
      [(xsig) 'application/vnd.openxmlformats-package.digital-signature-xmlsignature+xml]
      [else (iana-media-type-ref ext)])))

(define opc-override-type-name : (-> Symbol (Option Symbol))
  (lambda [t]
    (case t ; should be consistent with those in `opc-relation-type`
      [(App)          'application/vnd.openxmlformats-package.extended-properties+xml]
      [(Core)         'application/vnd.openxmlformats-package.core-properties+xml]
      [(document.xml) 'application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml]
      [else #false])))
