#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

(require "../ole/cfb.rkt")
(require "../ole/stream.rkt")

(require "ds/version.rkt")
(require "ds/map.rkt")
(require "ds/definition.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ms-crypto
  ([version : DataSpace-Version-Info]
   [map-entries : (Listof DataSpace-Map-Entry)]
   [transform-references : (Immutable-HashTable String (Listof String))])
  #:transparent
  #:type-name MS-Crypto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-crypto-compound-file : (-> Input-Port MS-Crypto)
  (lambda [/dev/cfbin]
    (define cfb (read-compound-file /dev/cfbin))

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
                (cdr e))))

      (writeln transform-references)

      (ms-crypto verinfo entries transform-references))))
