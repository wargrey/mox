#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-name : Symbol 'docx)

(struct mox-document
  ([body : XML-Document])
  #:type-name MOX-Document
  #:transparent)

(struct mox-word moxml
  ([main : MOX-Document]
   [glossary : MOX-Document])
  #:type-name MOX-Word
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-word-agent : (MOXML-Agentof MOX-Word)
  (lambda []
    (define-values (main-unzip main-realize) (moxml-document-agent))
    (define-values (glos-unzip glos-realize) (moxml-document-agent))
    
    (values docx-name

            (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [type : Symbol] [timestamp : (Option Natural) #false]] : (Option Void)
              (or (main-unzip entry dir? /dev/pkgin type timestamp)
                  (glos-unzip entry dir? /dev/pkgin type timestamp)

                  (case type
                    [else #false])))

            (λ [] : MOX-Word
              (mox-word (assert (main-realize)) (glos-realize))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-document-agent : (-> (Values MOXML-Unzip (MOXML-Realize MOX-Document)))
  (lambda []
    (define &doc : (Boxof (Option XML-Document)) (box #false))
    
    (values (λ [[entry : Bytes] [dir? : Boolean] [/dev/pkgin : Input-Port] [type : Symbol] [timestamp : (Option Natural) #false]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml)
                 (set-box! &doc (read-xml-document /dev/pkgin))]
                [else #false]))

            (λ [] : MOX-Document
              (mox-document (or (unbox &doc) (xml-blank docx-name)))))))
