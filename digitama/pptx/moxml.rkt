#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-name : Symbol 'pptx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-power-point moxml
  ([metadata : (Option XML-Document)])
  #:type-name MOX-PowerPoint
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-power-point-agent : (MOXML-Agentof MOX-PowerPoint)
  (lambda []
    #;(define-values (workbook-unzip workbook-realize) (moxml-workbook-agent))
    
    (values pptx-name

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [else #false]))

            (λ [] : MOX-PowerPoint
              (mox-power-point #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define moxml-workbook-agent : (-> (Values MOXML-Unzip (MOXML-Realize (Option Excel-Workbook))))
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
#;(define empty-workbook : Excel-Workbook
  (excel-workbook (xml-blank 'workbook)))
