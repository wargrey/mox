#lang typed/racket/base

(provide (all-defined-out))

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-print-width : Index 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define devimon-errno : (Parameterof Byte) (make-parameter 126))
(define devimon-skip-view : (Parameterof Boolean) (make-parameter #false))
(define devimon-verbose : (Parameterof Boolean) (make-parameter #false))
(define devimon-debug : (Parameterof Boolean) (make-parameter #false))
(define devimon-silent : (Parameterof Boolean) (make-parameter #false))
(define devimon-remake : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define devimon-restore-options! : (-> Void)
  (lambda []
    (devimon-errno 126)
    (devimon-skip-view #false)
    (devimon-verbose #false)
    (devimon-debug #false)
    (devimon-silent #false)
    (devimon-remake #false)
    (pretty-print-columns the-print-width)))
