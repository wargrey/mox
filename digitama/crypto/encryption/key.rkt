#lang typed/racket/base

(provide (all-defined-out))

(require digimon/number)
(require digimon/crypto)
(require digimon/dtrace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Encryption-Hash-Algorithm (Pairof EVP-MD-CTX* EVP-MD*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define encryption-select-hash : (-> Symbol (Option Encryption-Hash-Algorithm))
  (lambda [hash.alg]
    (define md* (evp-digest-fetch hash.alg))
    
    (or (and md*
             (cons (evp-digest-context-create) md*))
        (not (dtrace-error #:topic encryption-select-hash
                           "unsupported HASH algorithm: ~a"
                           hash.alg)))))

(define encryption-hash-size : (-> Encryption-Hash-Algorithm Index)
  (lambda [md]
    (evp-digest-size (cdr md))))

(define encryption-hash-block-size : (-> Encryption-Hash-Algorithm Index)
  (lambda [md]
    (evp-digest-block-size (cdr md))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define agile-key-generate : (-> Encryption-Hash-Algorithm Index Bytes Bytes Bytes Index Bytes)
  (lambda [shad spin salt passwd block-key key-bits]
    (define evp-md : EVP-MD* (cdr shad))
    (define mdctx : EVP-MD-CTX* (evp-digest-context-create))
    (define hsize : Index (encryption-hash-size shad))
    (define block-key : Bytes (make-bytes hsize))
    (define iterator : Bytes (make-bytes 4 0))

    (evp-digest-init mdctx evp-md)

    ; H_0 = H(salt+passwd)
    (evp-digest-update* mdctx (list salt passwd))
    
    ; H_n = H(iterator+H_n-1)
    (let generate ([it : Nonnegative-Fixnum 0])
      (when (< it spin)
        (evp-digest-update* mdctx (list iterator block-key))
        (evp-digest-final mdctx block-key)
        (evp-digest-init mdctx evp-md)
        (network-natural-bytes++ iterator)
        (generate (+ it 1))))

    ; H_final = H(H_n + block-key)
    (evp-digest-update* mdctx (list block-key block-key))
    (evp-digest-final mdctx block-key)

    (displayln (list '#:key hsize key-bits))

    block-key))

(define agile-initialization-vector-generate : (-> Encryption-Hash-Algorithm Bytes (Option Bytes) Index Bytes)
  (lambda [shad key-salt block-key block-size]
    (define hsize : Index (encryption-hash-size shad))
    (define digest : Bytes (make-bytes hsize))
    (define iterator : Bytes (make-bytes 4 0))

    (define IV : Bytes
      (if (and block-key)
          ; IV = H(key-salt+block-key)
          (let ([digest (make-bytes hsize)])
            (evp-digest (cdr shad) (list key-salt block-key) digest)
            digest)
          
          ; IV = key-salt
          key-salt))
    
    IV))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define encryption-bytes-ensure-size : (-> Bytes Index Byte Bytes)
  (lambda [src target-size pad]
    (define src-size : Index (bytes-length src))
    
    (cond [(< src-size target-size) (bytes-append src (make-bytes (- target-size src-size) pad))]
          [(> src-size target-size) src]
          [else src])))
