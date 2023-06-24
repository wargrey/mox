#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xexpr)

(require "main/media.rkt")
(require "main/extension.rkt")

(require "extLst.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->embedded-file : (-> XML-Element MOX#Embedded-File)
  (lambda [child]
    (define-values (embedded.wav _) (extract-mox#embedded-file (cadr child) (car child)))
    embedded.wav))

(define xml-element->media-file : (-> XML-Element (Option MOX-Media))
  (lambda [child]
    (case (car child)
      [(a:wavAudioFile) (xml-element->embedded-file child)]
      [(a:audioFile a:videoFile a:quickTimeFile)
       (let ([mime+ext (xml-element->attribute+art-extension-list child extract-mox#mime-file)])
         (make-mox-media-file #:type (xml-qname-local-part (car child))
                              #:mime (mox+extension-datum mime+ext)
                              #:extLst (mox+extension-extLst mime+ext)))]
      [(a:audioCd)
       (let*-values ([(cdtimes rest) (xml-empty-children->map* child extract-mox#cd-time)]
                     [(self) (make-mox-audio-cd #:st (hash-ref cdtimes 'a:st)
                                                #:end (hash-ref cdtimes 'a:end))])
         (cond [(or (null? rest) (not (eq? (caar rest) 'a:extLst))) self]
               [else (remake-mox-audio-cd self #:extLst (xml-element->art-extension-list (car rest)))]))]
      [else #false])))
