#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require sgml/sax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define agile-password-hash-input-block-key : Bytes #"\xfe\xa7\xd2\x76\x3b\x4b\x9e\x79")
(define agile-password-hash-value-block-key : Bytes #"\xd7\xaa\x0f\x6d\x30\x61\x34\x4e")
(define agile-password-key-value-block-key : Bytes #"\x14\x6e\x0b\xe7\xab\xac\xd0\xd6")

(define agile-data-integrity-salt-block-key : Bytes #"\x5f\xb2\xad\x01\x0c\xb9\xe1\xf6")
(define agile-data-integrity-hmac-block-key : Bytes #"\xA0\x67\x7f\x02\xb2\x2c\x84\x33")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct agile:enc () #:transparent #:type-name Enc:Attr)
(struct encryptor () #:transparent #:type-name Encryptor)

(define xml:attr-value->salt-size : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 1 65536)))

(define xml:attr-value->block-size : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 2 4096)))

(define xml:attr-value->key-bits : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 8)))

(define xml:attr-value->hash-size : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 1 65536)))

(define xml:attr-value->spin-count : (XML-Attribute-Value->Datum (Option Index))
  (lambda [v]
    (xml:attr-value->index v 0 10000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-attribute agile:key-data : Agile:Key-Data #:-> agile:enc
  #:with extract-agile:key-data agile:key-data->attributes
  ([saltSize : Index #:<-> xml:attr-value->salt-size]
   [blockSize : Index #:<-> xml:attr-value->block-size]
   [keyBits : Index #:<-> xml:attr-value->key-bits]
   [hashSize : Index #:<-> xml:attr-value->hash-size]
   [cipherAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [cipherChaining : Symbol #:<-> xml:attr-value->symbol]
   [hashAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [saltValue : String #:<-> xml:attr-value->string]))

(define-xml-attribute agile:data-integrity : Agile:Data-Integrity #:-> agile:enc
  #:with extract-agile:data-integrity agile:key-integrity->attributes
  ([encryptedHmacKey : String #:<-> xml:attr-value->string]
   [encryptedHmacValue : String #:<-> xml:attr-value->string]))

(define-xml-attribute agile:passwd-encryptor : Agile:Passwd-Encryptor #:-> encryptor
  #:with extract-agile:passwd-encryptor agile:passwd-encryptor->attributes
  ([saltSize : Index #:<-> xml:attr-value->salt-size]
   [blockSize : Index #:<-> xml:attr-value->block-size]
   [keyBits : Index #:<-> xml:attr-value->key-bits]
   [hashSize : Index #:<-> xml:attr-value->hash-size]
   [cipherAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [cipherChaining : Symbol #:<-> xml:attr-value->symbol]
   [hashAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [saltValue : String #:<-> xml:attr-value->string]
   [spinCount : Index #:<-> xml:attr-value->spin-count]
   [encryptedVerifierHashInput : String #:<-> xml:attr-value->string]
   [encryptedVerifierHashValue : String #:<-> xml:attr-value->string]
   [encryptedKeyValue : String #:<-> xml:attr-value->string]))

(define-xml-attribute agile:certificate-encryptor : Agile:Certificate-Encryptor #:-> encryptor
  #:with extract-agile:certificate-encryptor agile:certificate-encryptor->attributes
  ([encryptedKeyValue : String #:<-> xml:attr-value->string]
   [X509Certificate : String #:<-> xml:attr-value->string]
   [certVerifier : String #:<-> xml:attr-value->string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct agile-encryption-descriptor : Agile-Encryption-Descriptor
  ([key-data : Agile:Key-Data]
   [data-integrity : (Option Agile:Data-Integrity) #false]
   [encryptors : (Listof Encryptor) null])
  #:transparent)

(define read-agile-encryption-descriptor : (-> Input-Port Agile-Encryption-Descriptor)
  (lambda [/dev/encin]
    (read-xml-datum /dev/encin
                    agile-encryption-descriptor-handler
                    agile-encryption-descriptor-placeholder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define agile-encryption-descriptor-placeholder : Agile-Encryption-Descriptor
  (make-agile-encryption-descriptor
   #:key-data (make-agile:key-data #:saltSize 16 #:blockSize 16 #:keyBits 256 #:hashSize 64
                                   #:cipherAlgorithm 'AES #:cipherChaining 'ChainingModeCRC
                                   #:hashAlgorithm 'SHA512 #:saltValue "")))

(define agile-encryption-descriptor-element-handler : (XML-Element-Handler Agile-Encryption-Descriptor)
  (lambda [element xpath attrs ?empty ?preserve self]
    (if (list? attrs)
        (case element
          [(keyData)
           (let-values ([(keydata _) (extract-agile:key-data attrs element)])
             (remake-agile-encryption-descriptor self #:key-data keydata))]
          [(dataIntegrity)
           (let-values ([(integrity _) (extract-agile:data-integrity attrs element)])
             (remake-agile-encryption-descriptor self #:data-integrity integrity))]
          [(p:encryptedKey)
           (let ([encryptors (agile-encryption-descriptor-encryptors self)])
             (define-values (encryptor _)
               (if (memq 'keyEncryptor xpath)
                   (extract-agile:passwd-encryptor attrs element)
                   (extract-agile:certificate-encryptor attrs element)))
             (remake-agile-encryption-descriptor self #:encryptors (cons encryptor encryptors)))]
          [else self])
        self)))

(define agile-encryption-descriptor-handler : (XML-Event-Handlerof Agile-Encryption-Descriptor)
  ((inst make-xml-event-handler Agile-Encryption-Descriptor)
   #:element agile-encryption-descriptor-element-handler))
