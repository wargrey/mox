#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-drawingml moxml
  ([theme : (Option XML-Document)])
  #:type-name MOX-DrawingML
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-drawingml-agent : (MOXML-Agentof MOX-DrawingML)
  (lambda []
    (define &theme : (Boxof (Option XML-Document)) (box #false))
    
    (values 'drawingml

            (λ [[entry : Bytes] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.theme+xml)
                 (set-box! &theme (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [else #false]))

            (λ [] : MOX-DrawingML
              (mox-drawingml (unbox &theme))))))

