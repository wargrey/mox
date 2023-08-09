#lang typed/racket/base

(provide (all-defined-out))

(require digimon/port)

(require "cfb.rkt")
(require "tree.rkt")
(require "header.rkt")
(require "directory.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-stream : (-> Input-Port MS-CFB String [#:force-thread-safe? Boolean] (Option Input-Port))
  (lambda [/dev/cfbin cfb path #:force-thread-safe? [safe? #true]]
    (define object : (Option CFB-Object-Info) (assoc path (ms-cfb-objects cfb) string=?))
    (define cutoff-size : Index (cfb-header-stream-threshold-size (ms-cfb-header cfb)))

    (and (list? object)
         (let* ([size (caddr object)]
                [chain-head (cadddr object)]
                [ministream? (< size cutoff-size)])
           (open-input-block-chain #:total size #:name object #:force-thread-safe? safe?
                                   /dev/cfbin
                                   (if ministream?
                                       (cons (cfb-sector-chain chain-head (ms-cfb-minifat cfb))
                                             (ms-cfb-ministream-index->block cfb))
                                       (cons (cfb-sector-chain chain-head (ms-cfb-fat cfb))
                                             (ms-cfb-stream-index->block cfb)))
                                   #false)))))

(define #:forall (Datum) call-with-input-stream : (-> Input-Port MS-CFB String (-> Input-Port Datum)
                                                      [#:force-thread-safe? Boolean]
                                                      (Option Datum))
  (lambda [/dev/cfbin cfb path read #:force-thread-safe? [safe? #false]]
    (parameterize ([current-custodian (make-custodian)])
      (define /dev/blkin : (Option Input-Port)
        (open-input-stream #:force-thread-safe? safe?
                           /dev/cfbin cfb path))

      (dynamic-wind
       void
       (λ [] (and (input-port? /dev/blkin) (read /dev/blkin)))
       (λ [] (custodian-shutdown-all (current-custodian)))))))
