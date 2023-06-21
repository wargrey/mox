#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type MOX-Media (U MOX-Audio-CD MOX-Audio-File MOX#Embedded-File))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; QuickTime file doesn't have the `contentType` attribute
(define-mox-attribute mime-file #:for mox
  ([r:link : Symbol #:<-> mox:attr-value->relationship-id]
   [contentType : String #:= #false #:<-> xml:attr-value->string]))

; For embedded wav file
(define-mox-attribute embedded-file #:for mox
  ([r:embed : Symbol #:<-> mox:attr-value->relationship-id]
   [name : String #:<-> xml:attr-value->string]))

(define-mox-attribute cd-time #:for mox
  ([track : Byte #:<-> xml:attr-value->byte]
   [time : Index #:= #false #:<-> xml:attr-value->index]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct mox-audio-cd : MOX-Audio-CD
  ([st : MOX#Cd-Time]
   [end : MOX#Cd-Time]
   [extension : (Option MOX:Office-Art-Extension-List) #false])
  #:transparent)

(define-struct mox-audio-file : MOX-Audio-File
  ([attlist : MOX#Mime-File]
   [extension : (Option MOX:Office-Art-Extension-List) #false])
  #:transparent)
