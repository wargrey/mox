#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)
(require digimon/struct)

(require racket/string)
(require racket/symbol)

(require sgml/xexpr)

(require "partname.rkt")
(require "standards.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration mox-metainfo : MOX-Metainfo #:format "default-mox-~a"
  ([category : (Option String) #false] ; e.g. Resume, Letter, Financial Forecast, Proposal, Technical Presentation
   [status : (Option String) #false]   ; e.g. Draft, Reviewed, Final
   [type : (Option String) #false]     ; e.g. Whitepaper, Security Bulletin, Exam
   [created : (Option String) #false]
   [creator : (Option String) #false]
   [description : (Option String) #false]
   [identifier : (Option String) #false]
   [keywords : (Listof String) null]
   [language : (Option Symbol) #false]
   [last-modifier : (Option String) #false]
   [last-printed : (Option String) #false]
   [modified : (Option String) #false]
   [revision : (Option Natural) #false]
   [subject : (Option String) #false]
   [title : (Option String) #false]
   [version : (Option String) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-word-properties-markup-entries : (->* (String String String String String (Listof Any)) (#:utc (Option Integer)) Archive-Entry)
  (lambda [part-name-fmt title author version timestamp properties #:utc [ts #false]]
    (define metainfo : MOX-Metainfo
      (let ([?info (findf mox-metainfo? properties)])
        (cond [(mox-metainfo? ?info) ?info]
              [else (make-mox-metainfo)])))

    (define cores : (Listof (Option Xexpr))
      (list (select-string-value 'cp:category (mox-metainfo-category metainfo))
            (select-string-value 'cp:contentStatus (mox-metainfo-status metainfo))
            (select-string-value 'cp:contentType (mox-metainfo-type metainfo))
            (select-string-value 'dcterms:created (mox-metainfo-created metainfo))
            (select-string-value 'dc:creator (mox-metainfo-creator metainfo))
            (select-string-value 'dc:description (mox-metainfo-description metainfo))
            (select-string-value 'dc:identifier (mox-metainfo-identifier metainfo))
            (select-list-value 'keywords (mox-metainfo-keywords metainfo))
            (select-symbol-value 'dc:language (mox-metainfo-language metainfo))
            (select-string-value 'cp:lastModifiedBy (mox-metainfo-last-modifier metainfo))
            (select-string-value 'cp:lastPrinted (mox-metainfo-last-printed metainfo))
            (select-string-value 'dcterms:modified (mox-metainfo-modified metainfo) author)
            (select-natural-value 'cp:revision (mox-metainfo-revision metainfo))
            (select-string-value 'dc:subject (mox-metainfo-subject metainfo))
            (select-string-value 'dc:title (mox-metainfo-title metainfo) title)
            (select-string-value 'dc:version (mox-metainfo-version metainfo) version)))
    
    (define core-property.xml : Xexpr
      (list 'cp:coreProperties `([xmlns:cp . ,(assert (opc-xmlns 'Core:CP))]
                                 [xmlns:dcterms . ,(assert (opc-xmlns 'Core:DCTerms))]
                                 [xmlns:dcmitype . ,(assert (opc-xmlns 'Core:DCMIType))]
                                 [xmlns:dc . ,(assert (opc-xmlns 'Core:DC))]
                                 [xmlns:xsi . ,(assert (opc-xmlns 'Core:XSI))])
            (filter xexpr? cores)))

    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 10, 2006"
                              (xexpr->bytes core-property.xml #:prolog? #true)
                              (opc-part-name-normalize/zip (format part-name-fmt "core.xml")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-string-value : (->* (Symbol (Option String)) (String) (Option Xexpr))
  (lambda [tagname value [alt-value ""]]
    (define text : (Option String)
      (or (and value (not (string=? value "")) value)
          (and (not (string=? alt-value "")) alt-value)))

    (and text
         `(,tagname () (,text)))))

(define select-symbol-value : (-> Symbol (Option Symbol) (Option Xexpr))
  (lambda [tagname value]
    (and value (not (eq? value '||))
         `(,tagname ()
                    (,(symbol->immutable-string value))))))

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
