#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

(require "../ole/cfb.rkt")
(require "../ole/stream.rkt")

(require "version.rkt")
(require "dsmap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ms-crypto
  ([version : DataSpace-Version-Info]
   [map-entries : (Listof DataSpace-Map-Entry)])
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
      
      (for ([e (in-list entries)])
        (writeln e))

      (ms-crypto verinfo entries))))
