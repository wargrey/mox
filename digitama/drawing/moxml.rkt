#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-drawingml moxml
  ([theme : (Option XML-Document)]
   [table-style : (Option XML-Document)])
  #:type-name MOX-DrawingML
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-drawingml-agent : (MOXML-Agentof MOX-DrawingML)
  (lambda []
    (define &styleDef : (Boxof (Option XML-Document)) (box #false))
    (define &tblStyleLst : (Boxof (Option XML-Document)) (box #false))
    
    (values 'drawingml

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.theme+xml)
                 (set-box! &styleDef (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.tableStyles+xml)
                 (set-box! &tblStyleLst (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [else #false]))

            (λ [] : MOX-DrawingML
              (mox-drawingml (unbox &styleDef)
                             (unbox &tblStyleLst))))))

