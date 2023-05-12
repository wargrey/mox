#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xlsx-name : Symbol 'xlsx)

(struct excel-workbook
  ([entry : XML-Document])
  #:type-name Excel-Workbook
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-excel moxml
  ([workbook : Excel-Workbook]
   [strings : XML-Document]
   [chain : (Option XML-Document)]
   [chartsheets : (Listof XML-Document)]
   [connections : (Option XML-Document)]
   [metadata : (Option XML-Document)])
  #:type-name MOX-Excel
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-excel-agent : (MOXML-Agentof MOX-Excel)
  (lambda []
    (define-values (workbook-unzip workbook-realize) (moxml-workbook-agent))
    
    (values xlsx-name

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml)
                 (workbook-unzip entry type /dev/pkgin)]
                [else #false]))

            (λ [] : MOX-Excel
              (mox-excel (or (workbook-realize) empty-workbook)
                         (xml-blank xlsx-name)
                         #false null #false #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-workbook-agent : (-> (Values MOXML-Unzip (MOXML-Realize (Option Excel-Workbook))))
  (lambda []
    (define &xlsx : (Boxof (Option XML-Document)) (box #false))
    
    (values (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml)
                 (set-box! &xlsx (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [else #false]))

            (λ [] : (Option Excel-Workbook)
              (let ([xlsx (unbox &xlsx)])
                (and xlsx
                     (excel-workbook xlsx)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-workbook : Excel-Workbook
  (excel-workbook (xml-blank 'workbook)))
