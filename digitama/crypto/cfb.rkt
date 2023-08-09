#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)

(require digimon/stdio)

(require "../ole/cfb.rkt")
(require "../ole/stream.rkt")

(require "ds/version.rkt")
(require "ds/map.rkt")
(require "ds/definition.rkt")
(require "ds/transform.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ms-crypto
  ([version : DataSpace-Version-Info])
  #:transparent
  #:type-name MS-Crypto)

(struct ECMA-376 ms-crypto
  ([refcom : String]
   [transform : IRMDSTransformInfo])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-crypto-compound-file : (-> Input-Port MS-Crypto)
  (lambda [/dev/cfbin]
    (define cfb (read-compound-file /dev/cfbin))

    (for ([obj (in-list (ms-cfb-objects cfb))])
      (writeln obj))

    (parameterize ([default-stdin-locale "UTF-16LE"])
      (define verinfo : DataSpace-Version-Info
        (assert (call-with-input-stream /dev/cfbin cfb "/\u0006DataSpaces/Version"
                  read-ds-version-info)))
      
      (define entries : (Listof DataSpace-Map-Entry)
        (assert (call-with-input-stream /dev/cfbin cfb "/\u0006DataSpaces/DataSpaceMap"
                  read-ds-map-entries)))

      (define transform-references : (Immutable-HashTable String (Listof String))
        (read-ds-definition /dev/cfbin cfb
                            "/\u0006DataSpaces/DataSpaceInfo/"
                            (for/list : (Listof String) ([e (in-list entries)])
                              (car e))))

      (match (cons transform-references entries)
        [(list (hash-table ["StrongEncryptionDataSpace" (list "StrongEncryptionTransform")])
               (list "StrongEncryptionDataSpace"
                     (ds-map-refcom 'Stream (and "EncryptedPackage" refcom))))
         (read-crypto-ecma-376 /dev/cfbin cfb verinfo refcom)]
        [_ (ms-crypto verinfo)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-crypto-ecma-376 : (-> Input-Port MS-CFB DataSpace-Version-Info String ECMA-376)
  (lambda [/dev/cfbin cfb verinfo refcom]
    (define transform-info : IRMDSTransformInfo
      (assert (call-with-input-stream /dev/cfbin cfb
                "/\u0006DataSpaces/TransformInfo/StrongEncryptionTransform/\u0006Primary"
                read-IRMDS-transform-info)))
    
    (ECMA-376 verinfo refcom transform-info)))
