#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)
(require sgml/sax)

(require "../moxml.rkt")

(require "ml/pml.rkt")
(require "ml/presentation.rkt")
(require "ml/slide.rkt")
(require "ml/master/slide.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pptx-name : Symbol 'pptx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mox-powerpoint moxml
  ([presentation : PPTX-Presentation]
   [properties : XML-Document]
   [slides : (Listof PPTX-Slide)]
   [slide-layouts : (Listof XML-Document)]
   [slide-masters : (Listof PPTX-Slide-Master)]
   [view : (Option XML-Document)]
   [tags : (Listof XML-Document)]
   [comment-authors : (Option XML-Document)]
   [comments : (Option XML-Document)]
   [handout : (Option XML-Document)]
   [note-master : (Option XML-Document)]
   [note-slides : (Listof XML-Document)]
   [sync-data : (Listof XML-Document)])
  #:type-name MOX-PowerPoint
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define moxml-power-point-agent : (MOXML-Agentof MOX-PowerPoint)
  (lambda [pkg-type]
    (define &presentation : (Boxof PPTX-Presentation) (box default-presentation))
    (define &presentationPr : (Boxof XML-Document) (box empty-presentationPr))
    (define &slds : (Boxof (Listof PPTX-Slide)) (box null))
    (define &sldLayouts : (Boxof (Listof XML-Document)) (box null))
    (define &sldMasters : (Boxof (Listof PPTX-Slide-Master)) (box null))
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
                 (set-box! &presentation
                           (case pkg-type
                             [(text) (read-xml-datum /dev/pkgin pptx-presentation-text-sax-handler (unbox &presentation))]
                             [else (read-xml-datum /dev/pkgin pptx-presentation-sax-handler (unbox &presentation))]))]
                [(application/vnd.openxmlformats-officedocument.presentationml.presProps+xml
                  application/vnd.openxmlformats-officedocument.presentationml.presentationProperties+xml)
                 (set-box! &presentationPr (xml-document-normalize (read-xml-document /dev/pkgin)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slide+xml)
                 (let ([self (case pkg-type
                               [(text) (read-xml-datum /dev/pkgin pptx-slide-text-sax-handler default-slide)]
                               [else (read-xml-datum /dev/pkgin pptx-slide-sax-handler default-slide)])])
                   (set-box! &slds (cons self (unbox &slds))))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slideLayout+xml)
                 (set-box! &sldLayouts (cons (xml-document-normalize (read-xml-document /dev/pkgin)) (unbox &sldLayouts)))]
                [(application/vnd.openxmlformats-officedocument.presentationml.slideMaster+xml)
                 (let ([self (case pkg-type
                               [(text) (read-xml-datum /dev/pkgin pptx-slide-master-text-sax-handler default-slide-master)]
                               [else (read-xml-datum /dev/pkgin pptx-slide-master-sax-handler default-slide-master)])])
                   (set-box! &sldMasters (cons self (unbox &sldMasters))))]
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
                              (reverse (unbox &sldSyncPrs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-presentationPr : XML-Document (xml-blank 'presentationPr))
