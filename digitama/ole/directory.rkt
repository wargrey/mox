#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)
(require digimon/stdio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* cfb-directory-type #:+> CFB-Directory-Type
  directory-type->number number->directory-type
  [0 Empty User-Storage User-Stream Lock-Bytes Property Root])

(define-enumeration* cfb-directory-color #:+> CFB-Directory-Color
  directory-color->number number->directory-color
  [0 Red Black])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct cfb-directory-entry : CFB-Directory-Entry
  ([name : (#:locale 64)]
   [name-size+1/utf16 : LUInt16]
   [type : (#:enum Byte directory-type->number number->directory-type)]
   [color : (#:enum Byte directory-color->number number->directory-color)]
   [left-sibling : LUInt32 #:radix 16]
   [right-sibling : LUInt32 #:radix 16]
   [child : LUInt32 #:radix 16]
   [clsid : (#:raw 16) #:default #""]
   [user-flags : LUInt32 #:radix 2 #:default 0]
   [creation-time : LUInt64 #:default 0]
   [last-modification-time : LUInt64 #:default 0]
   [sector-chain-head : LUInt32]

   ;;; NOTE
   ; For version 3, the type should be of `LUInt32`,
   ;   with another not used field of 4 bytes.
   ; In this implementation, the most signifciant 32bit
   ;   of `stream-size` is ignored.
   [stream-size : LUInt64 #:radix 16]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-directory-name-size : (-> CFB-Directory-Entry Index)
  (lambda [dir]
    (define size (quotient (cfb-directory-entry-name-size+1/utf16 dir) 2))
    (if (> size 0) (- size 1) 0)))

(define cfb-directory-name : (-> CFB-Directory-Entry String)
  (lambda [dir]
    (substring (cfb-directory-entry-name dir)
               0
               (cfb-directory-name-size dir))))

(define cfb-directory-ci<? : (-> CFB-Directory-Entry CFB-Directory-Entry Boolean)
  (lambda [dir1 dir2]
    (cond [(< (cfb-directory-entry-name-size+1/utf16 dir1) (cfb-directory-entry-name-size+1/utf16 dir2)) #true]
          [(> (cfb-directory-entry-name-size+1/utf16 dir1) (cfb-directory-entry-name-size+1/utf16 dir2)) #false]
          [else (string-ci<? (cfb-directory-entry-name dir1) (cfb-directory-entry-name dir2))])))
