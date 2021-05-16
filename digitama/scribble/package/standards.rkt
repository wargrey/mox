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
      [(Core)          "http://schemas.openxmlformats.org/package/2006/metadata/core-properties"]
      [(Core:DCTerms)  "http://purl.org/dc/terms/"]
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
          [else (case type
                  [(Thumbnail)    "http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail"]
                  [(Core)         "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"]
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
    (case t
      [(document.xml) 'application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml]
      [else #false])))
