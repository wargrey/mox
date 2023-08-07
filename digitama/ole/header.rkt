#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #%cfb-identifier : Natural #xD0CF11E0A1B11AE1)
(define #%cfb-max-id : Natural #xFFFFFFFA)
#;(define #%cfb-reserved : Natural #xFFFFFFFB)
(define #%cfb-difat : Natural #xFFFFFFFC)
(define #%cfb-fat : Natural #xFFFFFFFD)
(define #%cfb-end-of-chain : Natural #xFFFFFFFE)
(define #%cfb-free : Natural #xFFFFFFFF)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct cfb-header : CFB-Header
  ([identifier : MUInt64 #:signature #%cfb-identifier]
   [clsid : (#:raw 16) #:default #""]
   [revision : LUInt16 #:radix 16 #:default #x003E]
   [version : LUInt16 #:radix 16 #:default #x0003]
   [byte-order : MUInt16 #:radix 16 #:default #xFEFF]
   [lnsector-size : LUInt16 #:default 9]
   [lnshort-sector-size : LUInt16 #:default 6]
   [0x22 : (#:reserved 6)]
   [directory-count : LUInt32 #:default 0]
   [fat-count : LUInt32]
   [directory-chain-head : LUInt32 #:radix 16]
   [transaction : (#:unused 4)]
   [stream-threshold-size : LUInt32 #:default 4096]
   [minifat-chain-head : LUInt32 #:radix 16]
   [minifat-count : LUInt32]
   [ext-difat-chain-head : LUInt32 #:radix 16]
   [ext-difat-count : LUInt32]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cfb-identifier-okay? : (-> Input-Port Boolean)
  (lambda [/dev/stdin]
    (equal? (peek-muint64 /dev/stdin)
            #%cfb-identifier)))

(define cfb-regular-id? : (-> Index Boolean)
  (lambda [secid]
    (< secid #%cfb-max-id)))
