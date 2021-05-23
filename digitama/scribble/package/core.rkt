#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)
(require digimon/struct)
(require digimon/tongue)

(require racket/string)
(require racket/symbol)

(require sgml/xexpr)

(require "partname.rkt")
(require "standards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration mox-metainfo : MOX-Metainfo #:format "default-mox-~a"
  (; /docProps/core.xml
   [category : (Option String) #false] ; e.g. Resume, Letter, Financial Forecast, Proposal, Technical Presentation
   [status : (Option String) #false]   ; e.g. Draft, Reviewed, Final
   [type : (Option String) #false]     ; e.g. Whitepaper, Security Bulletin, Exam
   [created : (Option String) #false]
   [creator : (Option String) #false]
   [comment : (Option String) #false]
   [identifier : (Option String) #false]
   [keywords : (Listof String) null]
   [language : (Option Symbol) #false]
   [last-modifier : (Option String) #false]
   [last-printed : (Option String) #false]
   [modified : (Option String) #false]
   [revision : (Option Natural) #false]
   [subject : (Option String) #false]
   [title : (Option String) #false]
   [version : (Option String) #false]

   ; /docProps/app.xml
   [manager : (Option String) #false]
   [company : (Option String) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-core-properties-markup-entries : (->* (String MOX-Metainfo String String String String) (#:utc (Option Integer)) Archive-Entry)
  (lambda [part-name-fmt metainfo title author version timestamp #:utc [ts #false]]
    (define dcterms-attlist : Xexpr-AttList
      `([xsi:type . "dcterms:W3CDTF"]))
    
    (define cores : (Listof (Option Xexpr))
      (list (select-string-value  'dc:title (mox-metainfo-title metainfo) title)
            (select-string-value  'dc:subject (mox-metainfo-subject metainfo))
            (select-string-value  'dc:creator (mox-metainfo-creator metainfo) author)
            (select-list-value    'cp:keywords (mox-metainfo-keywords metainfo))
            (select-string-value  'dc:description (mox-metainfo-comment metainfo))
            (select-string-value  'cp:lastModifiedBy (mox-metainfo-last-modifier metainfo))
            (select-natural-value 'cp:revision (mox-metainfo-revision metainfo))
            (select-string-value  'cp:lastPrinted (mox-metainfo-last-printed metainfo))
            (select-string-value  'dcterms:created (mox-metainfo-created metainfo) #:attlist dcterms-attlist)
            (select-string-value  'dcterms:modified (mox-metainfo-modified metainfo) timestamp #:attlist dcterms-attlist)
            (select-string-value  'cp:category (mox-metainfo-category metainfo))
            (select-string-value  'dc:identifier (mox-metainfo-identifier metainfo))
            (select-symbol-value  'dc:language (mox-metainfo-language metainfo) (current-tongue))
            (select-string-value  'cp:contentStatus (mox-metainfo-status metainfo))
            (select-string-value  'cp:contentType (mox-metainfo-type metainfo))
            (select-string-value  'dc:version (mox-metainfo-version metainfo) version)))
    
    (define core-property.xml : Xexpr
      (list 'cp:coreProperties `([xmlns:cp . ,(assert (opc-xmlns 'Core:CP))]
                                 [xmlns:dcmitype . ,(assert (opc-xmlns 'Core:DCMIType))]
                                 [xmlns:dcterms . ,(assert (opc-xmlns 'Core:DCTerms))]
                                 [xmlns:dc . ,(assert (opc-xmlns 'Core:DC))]
                                 [xmlns:xsi . ,(assert (opc-xmlns 'Core:XSI))])
            (filter xexpr? cores)))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 10, 2006"
                              (xexpr->bytes core-property.xml #:prolog? #true)
                              (opc-part-name-normalize/zip (format part-name-fmt "core.xml")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-properties-metainfo-ref : (-> (Listof Any) MOX-Metainfo)
  (lambda [properties]
    (let ([?info (findf mox-metainfo? properties)])
      (cond [(mox-metainfo? ?info) ?info]
            [else (make-mox-metainfo)]))))

(define mox-shared-application-properties-xexprs : (-> MOX-Metainfo String Any (Listof Xexpr))
  (lambda [metainfo application appversion]
    (define apps : (Listof (Option Xexpr))
      (list `(Application () (,application))

            ; TODO: why these two properties don't work
            (select-string-value 'Manager (mox-metainfo-manager metainfo))
            (select-string-value 'Company (mox-metainfo-company metainfo))

            `(AppVersion () (,(format "~a" appversion)))))
    
    (filter xexpr? apps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-string-value : (->* (Symbol (Option String)) (String #:attlist Xexpr-AttList) (Option Xexpr))
  (lambda [tagname value [alt-value ""] #:attlist [attlist null]]
    (define text : (Option String)
      (or (and value (not (string=? value "")) value)
          (and (not (string=? alt-value "")) alt-value)))

    (and text
         `(,tagname ,attlist (,text)))))

(define select-symbol-value : (->* (Symbol (Option Symbol)) (Symbol) (Option Xexpr))
  (lambda [tagname value [alt-value '||]]
    (define s : (Option Symbol)
      (or (and value (not (eq? value '||)) value)
          (and (not (eq? alt-value '||)) alt-value)))
    
    (and s
         `(,tagname ()
                    (,(symbol->immutable-string s))))))

(define select-natural-value : (-> Symbol (Option Natural) (Option Xexpr))
  (lambda [tagname value]
    (and value
         `(,tagname ()
                    (,(number->string value))))))

(define select-list-value : (-> Symbol (Option (Listof String)) (Option Xexpr))
  (lambda [tagname value-list]
    (define text : (Option String)
      (and (pair? value-list)
           (string-join value-list ", ")))

    (and text
         `(,tagname () (,text)))))
