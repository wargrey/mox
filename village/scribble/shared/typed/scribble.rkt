#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/typed/provide
 scribble/core
 [#:struct document-date ([text : String]) #:extra-constructor-name make-document-date]
 [#:struct document-version ([text : String]) #:extra-constructor-name make-document-version]
 [#:struct style ([name : Style-Name] [properties : (Listof Any)]) #:extra-constructor-name make-style])

(require/typed/provide
 scribble/html-properties
 [#:struct body-id ([value : String]) #:extra-constructor-name make-body-id]
 [#:struct hover-property ([text : String]) #:extra-constructor-name make-hover-property])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Style-Name (U String Symbol False))
(define-type Style-Properties (Listof Any))

(define-type Element-Style (U Style-Name style))
