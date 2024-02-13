#lang typed/racket/base

(provide (all-defined-out))

(require digimon/port)
(require digimon/binscii)

(require "cfb.rkt")
(require "parameter.rkt")

(require "encryption.rkt")
(require "encryption/key.rkt")
(require "encryption/agile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-encrypted-input-stream : (-> Input-Port MS-Crypto (Option Input-Port))
  (lambda [/dev/cfbin crypto]
    (and (ECMA-376? crypto)
         (let ([enc-info (ECMA-376-encryption crypto)])
           (and (Agile-Encryption? enc-info)
                (open-agile-encrypted-input-stream /dev/cfbin (Agile-Encryption-desc enc-info)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-agile-encrypted-input-stream : (-> Input-Port Agile-Encryption-Descriptor (Option Input-Port))
  (lambda [/dev/cfbin encryption]
    (define key-data : Agile:Key-Data
      (agile-encryption-descriptor-key-data encryption))
    (define data-integrity : (Option Agile:Data-Integrity)
      (agile-encryption-descriptor-data-integrity encryption))
    (define encryptors : (Listof Encryptor)
      (reverse (agile-encryption-descriptor-encryptors encryption)))

    (for/or : (Option Input-Port) ([encryptor (in-list encryptors)])
      (cond [(agile:passwd-encryptor? encryptor)
             (writeln encryptor)
             (let ([shad (encryption-select-hash (agile:passwd-encryptor-hashAlgorithm encryptor))]
                   [spin (agile:passwd-encryptor-spinCount encryptor)]
                   [salt (base64-decode (agile:passwd-encryptor-saltValue encryptor))]
                   [bkey agile-password-hash-input-block-key]
                   [kbits (agile:passwd-encryptor-keyBits encryptor)])
               (and shad
                    (let ([passwd (string->bytes/utf-8 ((default-ole-ask-password) (object-name /dev/cfbin)))])
                      (define EK (agile-key-generate shad spin salt passwd bkey kbits))
                      (displayln EK)
                      #false)))]
            [else #false]))))
