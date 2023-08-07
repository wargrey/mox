#lang typed/racket/base

(provide (all-defined-out))

(require digimon/port)

(require "cfb.rkt")
(require "tree.rkt")
(require "header.rkt")
(require "directory.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-input-stream : (-> Input-Port MS-CFB String (Option Input-Port))
  (lambda [/dev/cfbin cfb path]
    (define object : (Option CFB-Object-Info) (assoc path (ms-cfb-objects cfb) string=?))
    (define cutoff-size : Index (cfb-header-stream-threshold-size (ms-cfb-header cfb)))

    (and (list? object)
         (let* ([size (cadr object)]
                [chain-head (caddr object)]
                [ministream? (< size cutoff-size)])
           (open-input-block-chain #:total size #:name object
                                   /dev/cfbin
                                   (if ministream?
                                       (cons (cfb-sector-chain chain-head (ms-cfb-minifat cfb))
                                             (ms-cfb-ministream-index->block cfb))
                                       (cons (cfb-sector-chain chain-head (ms-cfb-fat cfb))
                                             (ms-cfb-stream-index->block cfb)))
                                   #false)))))
