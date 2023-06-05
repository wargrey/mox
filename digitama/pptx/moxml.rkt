#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../moxml.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-name : Symbol 'pptx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-powerpoint moxml
  ([presentation : XML-Document]
   [properties : XML-Document]
   [slides : (Listof XML-Document)]
   [slide-layouts : (Listof XML-Document)]
   [slide-masters : (Listof XML-Document)]
   [view : (Option XML-Document)]
   [tags : (Listof XML-Document)]
   [comment-authors : (Option XML-Document)]
   [comments : (Option XML-Document)]
   [handout : (Option XML-Document)]
   [note-master : (Option XML-Document)]
   [slide-notes : (Listof XML-Document)]
   [sync-data : (Listof XML-Document)])
  #:type-name MOX-PowerPoint
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-power-point-agent : (MOXML-Agentof MOX-PowerPoint)
  (lambda []
    (define &presentation : (Boxof XML-Document) (box empty-presentation))
    (define &presentationPr : (Boxof XML-Document) (box empty-presentationPr))
    (define &slds : (Boxof (Listof XML-Document)) (box null))
    (define &sldLayouts : (Boxof (Listof XML-Document)) (box null))
    (define &sldMasters : (Boxof (Listof XML-Document)) (box null))
    (define &sldSyncPrs : (Boxof (Listof XML-Document)) (box null))
    (define &viewPr : (Boxof (Option XML-Document)) (box #false))
    (define &tagLst : (Boxof (Listof XML-Document)) (box null))
    (define &cmAuthorLst : (Boxof (Option XML-Document)) (box #false))
    (define &cmLst : (Boxof (Option XML-Document)) (box #false))
    (define &handoutMaster : (Boxof (Option XML-Document)) (box #false))
    (define &notesMaster : (Boxof (Option XML-Document)) (box #false))
    (define &notes : (Boxof (Listof XML-Document)) (box null))
    
    (values pptx-name

            (λ [[entry : String] [type : Symbol] [/dev/pkgin : Input-Port]] : (Option Void)
              (case type
                [(application/vnd.openxmlformats-officedocument.presentationml.presentation.main+xml
                  application/vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml
                  application/vnd.openxmlformats-officedocument.presentationml.template.main+xml)
                 (set-box! &presentation (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.presentationProperties+xml)
                 (set-box! &presentationPr (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slide+xml)
                 (set-box! &slds (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &slds)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml)
                 (set-box! &sldLayouts (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &sldLayouts)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml)
                 (set-box! &sldMasters (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &sldMasters)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.tags+xml)
                 (set-box! &tagLst (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &tagLst)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slideUpdateInfo+xml)
                 (set-box! &sldSyncPrs (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &sldSyncPrs)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.viewProps+xml)
                 (set-box! &viewPr (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.commentAuthors+xml)
                 (set-box! &cmAuthorLst (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.comments+xml)
                 (set-box! &cmLst (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.handoutMaster+xml)
                 (set-box! &handoutMaster (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.notesMaster+xml)
                 (set-box! &notesMaster (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.notesSlide+xml)
                 (set-box! &notes (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &notes)))]
                [else #false]))

            (λ [] : MOX-PowerPoint
              (mox-powerpoint (unbox &presentation) (unbox &presentationPr)
                              (reverse (unbox &slds)) (reverse (unbox &sldLayouts)) (reverse (unbox &sldMasters))
                              (unbox &viewPr)
                              (reverse (unbox &tagLst))
                              (unbox &cmAuthorLst) (unbox &cmLst)
                              (unbox &handoutMaster)
                              (unbox &notesMaster) (reverse (unbox &notes))
                              (reverse (unbox &sldMasters)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-presentation : XML-Document (xml-blank 'presentation))
(define empty-presentationPr : XML-Document (xml-blank 'presentationPr))
