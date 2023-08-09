#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)
(require digimon/enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* ds-transform-type #:+> DS-Transform-Type
  ds-transform-type->number number->ds-transform-type
  [1 ~UniqueType])

(define-binary-struct transform-header : Transform-Header
  ([length : LUInt32]
   [type : (#:enum LUInt32 ds-transform-type->number number->ds-transform-type)]
   [id : (LNLocale 4)]
   [name : (LNLocale 4)]
   [reader-version : LUInt32 #:radix 16]
   [updater-version : LUInt32 #:radix 16]
   [writer-version : LUInt32 #:radix 16]))

(define-binary-struct extensibility-header : Extensibility-Header
  ([length : LUInt32]))

(define-binary-struct transform-cipher : Transform-Info
  ([name : (LNString 4)]
   [blocksize : LUInt32 #:default #x10]
   [mode : LUInt32 #:default #x00]
   [reserved : LUInt32 #:default #x04]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct IRMDSTransformInfo
  ([header : Transform-Header]
   [ext : Extensibility-Header]
   [user-license : String])
  #:transparent)

(define read-IRMDS-transform-info : (-> Input-Port IRMDSTransformInfo)
  (lambda [/dev/trin]
    (define header : Transform-Header (read-transform-header /dev/trin))
    (define ext : Extensibility-Header (read-extensibility-header /dev/trin))
    (define user-license : String (read-ln:bstring /dev/trin 4))
    
    (IRMDSTransformInfo header ext user-license)))
