#lang typed/racket/base

(provide (all-defined-out))

(require racket/port)
(require racket/symbol)

(require sgml/digitama/document)
(require sgml/digitama/namespace)
(require sgml/digitama/plain/sax)

(require "property.rkt")

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-sharedml moxml
  ([properties : MOX-File-Properties]
   [custom-properties : (Option MOX-Custom-Properties)])
  #:type-name MOX-SharedML)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-sharedml-agent : (MOXML-Agentof MOX-SharedML)
  (lambda []
    (define &cprops-xmlns : (Boxof (Option String)) (box #false))
    (define file-properties : MOX-File-Properties (make-hasheq))
    (define custom-properties : (HashTable Symbol MOX-Custom-Property) (make-hasheq))
    
    (values 'sharedml

            (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [type : Symbol] [timestamp : (Option Natural) #false]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-package.core-properties+xml application/vnd.openxmlformats-officedocument.extended-properties+xml)
                 (load-xml-datum /dev/pkgin (make-file-properties-sax-handler file-properties))]
                [(application/vnd.openxmlformats-officedocument.custom-properties+xml)
                 (load-xml-datum /dev/pkgin (make-custom-properties-sax-handler &cprops-xmlns custom-properties))]
                [else #false]))

            (λ [] : MOX-SharedML
              (mox-sharedml file-properties
                            (let ([vt (unbox &cprops-xmlns)])
                              (and (string? vt)
                                   (mox-custom-properties vt custom-properties))))))))
