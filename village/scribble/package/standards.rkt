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
      [(App)           "http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"]
      [(App:VT)        "http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"]
      [(Core:CP)       "http://schemas.openxmlformats.org/package/2006/metadata/core-properties"]
      [(Core:DCTerms)  "http://purl.org/dc/terms/"]
      [(Core:DCMIType) "http://purl.org/dc/dcmityp/"]
      [(Core:DC)       "http://purl.org/dc/elements/1.1/"]
      [(Core:XSI)      "http://www.w3.org/2001/XMLSchema-instance"]
      [(Signatures)    "http://schemas.openxmlformats.org/package/2006/digital-signature"]
      [(Relationships) "http://schemas.openxmlformats.org/package/2006/relationships"]
      [(Compatibility) "http://schemas.openxmlformats.org/markup-compatibility/2006"]

      ; for Word document
      [(Docx:W)        "http://schemas.openxmlformats.org/wordprocessingml/2006/main"]
      [(Docx:W10)      "urn:schemas-microsoft-com:office:word"]
      [(Docx:W14)      "http://schemas.microsoft.com/office/word/2010/wordml"]
      [(Docx:W15)      "http://schemas.microsoft.com/office/word/2012/wordml"]
      [(Docx:W16)      "http://schemas.microsoft.com/office/word/2018/wordml"]
      [(Docx:W16se)    "http://schemas.microsoft.com/office/word/2015/wordml/symex"]
      [(Docx:W16cid)   "http://schemas.microsoft.com/office/word/2016/wordml/cid"]
      [(Docx:W16cex)   "http://schemas.microsoft.com/office/word/2018/wordml/cex"]
      [(Docx:W16sdtdh) "http://schemas.microsoft.com/office/word/2020/wordml/sdtdatahash"]
      [(Docx:WP)       "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"]
      [(Docx:WP14)     "http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"]
      [(Docx:WPC)      "http://schemas.microsoft.com/office/word/2010/wordprocessingCanvas"]
      [(Docx:WPI)      "http://schemas.microsoft.com/office/word/2010/wordprocessingInk"]
      [(Docx:WPG)      "http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"]
      [(Docx:WPS)      "http://schemas.microsoft.com/office/word/2010/wordprocessingShape"]
      [(Docx:WNE)      "http://schemas.microsoft.com/office/word/2006/wordml"]
      
      ; for shared ML
      [(Draw:CX)       "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX1)      "http://schemas.microsoft.com/office/drawing/2015/9/8/chartex"]
      [(Draw:CX2)      "http://schemas.microsoft.com/office/drawing/2015/10/21/chartex"]
      [(Draw:CX3)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX4)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX5)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX6)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX7)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:CX8)      "http://schemas.microsoft.com/office/drawing/2014/chartex"]
      [(Draw:AINK)     "http://schemas.microsoft.com/office/drawing/2016/ink"]
      [(Draw:AM3D)     "http://schemas.microsoft.com/office/drawing/2017/model3d"]
      [(Draw:O)        "urn:schemas-microsoft-com:office:office"]
      [(Draw:R)        "http://schemas.openxmlformats.org/officeDocument/2006/relationships"]
      [(Draw:M)        "http://schemas.openxmlformats.org/officeDocument/2006/math"]
      [(Draw:V)        "urn:schemas-microsoft-com:vml"]

      [else #false])))

(define opc-relationship-type : (-> (U String Symbol) String)
  (lambda [type]
    (cond [(string? type) type]
          [else (case type ; should be consistent with those in `opc-override-type-name`
                  [(App)             "http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"]
                  [(Core)            "http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"]
                  [(Thumbnail)       "http://schemas.openxmlformats.org/package/2006/relationships/metadata/thumbnail"]
                  [(Certificate)     "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/certificate"]
                  [(Signature)       "http://schemas.openxmlformats.org/package/2006/relationships/digital-signature/origin"]
                  [(document.xml)    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"]
                  [(styles.xml)      "http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"]
                  [(fontTable.xml)   "http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable"]
                  [(theme.xml)       "http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme"]
                  [(footnotes.xml)   "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes"]
                  [(endnotes.xml)    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes"]
                  [(settings.xml)    "http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings"]
                  [(webSettings.xml) "http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings"]
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
      [(App)             'application/vnd.openxmlformats-officedocument.extended-properties+xml]
      [(Core)            'application/vnd.openxmlformats-package.core-properties+xml]
      [(document.xml)    'application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml]
      [(styles.xml)      'application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml]
      [(fontTable.xml)   'application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml]
      [(theme.xml)       'application/vnd.openxmlformats-officedocument.theme+xml]
      [(footnotes.xml)   'application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml]
      [(endnotes.xml)    'application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml]
      [(settings.xml)    'application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml]
      [(webSettings.xml) 'application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml]
      [else #false])))
