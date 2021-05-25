#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require "../package/partname.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-style-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.12, 2006"
                             (collection-file-path "default.xml" "mox" "stone" "docx" "style")
                             (opc-part-name-normalize/zip part-name))))

(define opc-word-theme-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 14.2.7, 2006"
                             (collection-file-path "theme.xml" "mox" "stone" "docx" "style")
                             (opc-part-name-normalize/zip part-name))))
