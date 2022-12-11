#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &n : (Boxof Integer) (box 0))

(define datatype-reset! : (-> Void)
  (lambda []
    (set-box! &n 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define st-decimal-number : (->* () ((Option Integer)) Integer)
  (lambda [[n0 #false]]
    (define n : Integer (or n0 (add1 (unbox &n))))
    
    (set-box! &n n)
    n))
