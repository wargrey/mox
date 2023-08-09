#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require sgml/sax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct enc:attr () #:transparent #:type-name Enc:Attr)
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
(define-xml-attribute enc:attr:key-data : Enc:Attr:Key-Data #:-> enc:attr
  #:with extract-enc:attr:key-data enc:attr:key-data->attributes
  ([saltSize : Index #:<-> xml:attr-value->salt-size]
   [blockSize : Index #:<-> xml:attr-value->block-size]
   [keyBits : Index #:<-> xml:attr-value->key-bits]
   [hashSize : Index #:<-> xml:attr-value->hash-size]
   [cipherAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [cipherChaining : Symbol #:<-> xml:attr-value->symbol]
   [hashAlgorithm : Symbol #:<-> xml:attr-value->symbol]
   [saltValue : String #:<-> xml:attr-value->string]))

(define-xml-attribute enc:attr:data-integrity : Enc:Attr:Data-Integrity #:-> enc:attr
  #:with extract-enc:attr:data-integrity enc:attr:key-integrity->attributes
  ([encryptedHmacKey : String #:<-> xml:attr-value->string]
   [encryptedHmacValue : String #:<-> xml:attr-value->string]))

(define-xml-attribute enc:attr:passwd-encryptor : Enc:Attr:Passwd-Encryptor #:-> encryptor
  #:with extract-enc:attr:passwd-encryptor enc:attr:passwd-encryptor->attributes
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

(define-xml-attribute enc:attr:certificate-encryptor : Enc:Attr:Certificate-Encryptor #:-> encryptor
  #:with extract-enc:attr:certificate-encryptor enc:attr:certificate-encryptor->attributes
  ([encryptedKeyValue : String #:<-> xml:attr-value->string]
   [X509Certificate : String #:<-> xml:attr-value->string]
   [certVerifier : String #:<-> xml:attr-value->string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct agile-encryption-descriptor : Agile-Encryption-Descriptor
  ([key-data : Enc:Attr:Key-Data]
   [data-integrity : (Option Enc:Attr:Data-Integrity) #false]
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
   #:key-data (make-enc:attr:key-data #:saltSize 16 #:blockSize 16 #:keyBits 256 #:hashSize 64
                                      #:cipherAlgorithm 'AES #:cipherChaining 'ChainingModeCRC
                                      #:hashAlgorithm 'SHA512 #:saltValue "")))

(define agile-encryption-descriptor-element-handler : (XML-Element-Handler Agile-Encryption-Descriptor)
  (lambda [element xpath attrs ?empty ?preserve self]
    (if (list? attrs)
        (case element
          [(keyData)
           (let-values ([(keydata _) (extract-enc:attr:key-data attrs element)])
             (remake-agile-encryption-descriptor self #:key-data keydata))]
          [(dataIntegrity)
           (let-values ([(integrity _) (extract-enc:attr:data-integrity attrs element)])
             (remake-agile-encryption-descriptor self #:data-integrity integrity))]
          [(p:encryptedKey)
           (let ([encryptors (agile-encryption-descriptor-encryptors self)])
             (define-values (encryptor _)
               (if (memq 'keyEncryptor xpath)
                   (extract-enc:attr:passwd-encryptor attrs element)
                   (extract-enc:attr:certificate-encryptor attrs element)))
             (remake-agile-encryption-descriptor self #:encryptors (cons encryptor encryptors)))]
          [else self])
        self)))

(define agile-encryption-descriptor-handler : (XML-Event-Handlerof Agile-Encryption-Descriptor)
  ((inst make-xml-event-handler Agile-Encryption-Descriptor)
   #:element agile-encryption-descriptor-element-handler))
