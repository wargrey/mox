#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)
(require digimon/enumeration)

(require "encryption/agile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* encryption-algorithm #:+> Encryption-Algorithm
  encryption-algorithm->number number->encryption-algorithm
  [CheckFlags #x00] [RC4 #x6801]
  [AES128 #x660E] [AES192 #x660F] [AES256 #x6610])

(define-binary-struct encryption-header : Encryption-Header
  ([flags : LUInt32 #:radix 2]
   [extra-size : LUInt32]
   [algid : (#:enum LUInt32 encryption-algorithm->number number->encryption-algorithm)]
   [algid-hash : LUInt32 #:radix 16]
   [key-size : LUInt32]
   [provider : LUInt32 #:radix 16]
   [reserved1 : (#:unused 4)]
   [reserved2 : LUInt32 #:default 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct Encryption-Info () #:transparent)

(struct Standard-Encryption Encryption-Info () #:transparent)
(struct Extensible-Encryption Encryption-Info () #:transparent)

(struct Agile-Encryption Encryption-Info
  ([reversed : Index]
   [desc : Agile-Encryption-Descriptor])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-encryption-info : (-> Input-Port (Option Encryption-Info))
  (lambda [/dev/cfbin]
    (define vmajor (read-luint16 /dev/cfbin))
    (define vminor (read-luint16 /dev/cfbin))

    (cond
      [(= vminor #x0004) (read-agile-encryption-info /dev/cfbin)]
      [(= vminor #x0003) #false]
      [(= vminor #x0002) #false]
      [else #false])))

(define read-agile-encryption-info : (-> Input-Port Agile-Encryption)
  (lambda [/dev/cfbin]
    (define whocare : Index (read-luint32 /dev/cfbin))
    (define aed : Agile-Encryption-Descriptor (read-agile-encryption-descriptor /dev/cfbin))

    (Agile-Encryption whocare aed)))
