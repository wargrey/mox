#lang typed/racket/base

(provide (all-defined-out))

(require "../../../dialect.rkt")
(require "../../../shared/ml/common-simple-types.rkt")

(require "media.rkt")
(require "extension.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-mox-element hyperlink #:for mox
  #:attlist
  ([r:id : Symbol #:<-> mox:attr-value->relationship-id]
   [invalidUrl : String #:= #false #:<-> xml:attr-value->uri-string]
   [action : String #:= #false #:<-> xml:attr-value->string]
   [tgtFrame : String #:= #false #:<-> xml:attr-value->string]
   [tooltip : String #:= #false #:<-> xml:attr-value->string]
   [history : XML-Boolean #:= [#false 'true] #:<-> xml:attr-value->boolean]
   [highlightClick : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean]
   [endSnd : XML-Boolean #:= [#false 'false] #:<-> xml:attr-value->boolean])
  ([snd : (Option MOX#Embedded-File) #false]
   [extLst : (Option MOX:Office-Art-Extension-List) #false]))
