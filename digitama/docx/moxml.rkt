#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-name : Symbol 'docx)

(struct word-document
  ([body : XML-Document]
   [docsettings : (Option XML-Document)]
   [websettings : (Option XML-Document)]
   [fonts : (Option XML-Document)]
   [numbering : (Option XML-Document)]
   [styles : (Option XML-Document)]
   [comments : (Option XML-Document)]
   [footnotes : (Option XML-Document)]
   [endnotes : (Option XML-Document)])
  #:type-name Word-Document
  #:transparent)

(struct mox-word moxml
  ([main : Word-Document]
   [glossary : (Option Word-Document)]
   [header : (Option XML-Document)]
   [footer : (Option XML-Document)])
  #:type-name MOX-Word
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-word-agent : (MOXML-Agentof MOX-Word)
  (lambda []
    (define-values (main-unzip main-realize) (moxml-document-agent))
    (define-values (glos-unzip glos-realize) (moxml-document-agent))
    (define &header : (Boxof (Option XML-Document)) (box #false))
    (define &footer : (Boxof (Option XML-Document)) (box #false))
    
    (values docx-name

            (λ [[entry : Bytes] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml)
                 (main-unzip entry type /dev/pkgin)]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml)
                 (glos-unzip entry type /dev/pkgin)]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml)
                 (if (docx-glossary? entry)
                     (glos-unzip entry type /dev/pkgin)
                     (main-unzip entry type /dev/pkgin))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml)
                 (set-box! &header (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml)
                 (set-box! &footer (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [else #false]))

            (λ [] : MOX-Word
              (mox-word (or (main-realize) docx-blank) (glos-realize)
                        (unbox &header) (unbox &footer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-document-agent : (-> (Values MOXML-Unzip (MOXML-Realize (Option Word-Document))))
  (lambda []
    (define &doc : (Boxof (Option XML-Document)) (box #false))
    (define &docsettings : (Boxof (Option XML-Document)) (box #false))
    (define &websettings : (Boxof (Option XML-Document)) (box #false))
    (define &fonts : (Boxof (Option XML-Document)) (box #false))
    (define &numbering : (Boxof (Option XML-Document)) (box #false))
    (define &styles : (Boxof (Option XML-Document)) (box #false))
    (define &comments : (Boxof (Option XML-Document)) (box #false))
    (define &itemdefs : (Boxof (Option XML-Document)) (box #false))
    (define &footnotes : (Boxof (Option XML-Document)) (box #false))
    (define &endnotes : (Boxof (Option XML-Document)) (box #false))
    
    (values (λ [[entry : Bytes] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml
                  application/vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml)
                 (set-box! &doc (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml)
                 (set-box! &fonts (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml)
                 (set-box! &numbering (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml)
                 (set-box! &styles (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml)
                 (set-box! &footnotes (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml)
                 (set-box! &endnotes (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml)
                 (set-box! &docsettings (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml)
                 (set-box! &websettings (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml)
                 (set-box! &comments (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [else #false]))

            (λ [] : (Option Word-Document)
              (let ([docx (unbox &doc)])
                (and docx
                     (word-document docx
                                    (unbox &docsettings) (unbox &websettings)
                                    (unbox &fonts) (unbox &numbering) (unbox &styles)
                                    (unbox &comments) (unbox &footnotes) (unbox &endnotes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-blank : Word-Document
  (word-document (xml-blank docx-name)
                 #false #false
                 #false #false #false
                 #false #false #false))

(define docx-glossary? : (-> Bytes Boolean)
  (let ([px.glossary #px"^word/glossary/"])
    (lambda [entry]
      (regexp-match? px.glossary entry))))
