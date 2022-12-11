#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require "../package/partname.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-footnote-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.7, 2006"
                             (collection-file-path "footnotes.xml" "mox" "stone" "docx" "misc")
                             (opc-part-name-normalize/zip part-name))))

(define opc-word-endnote-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.4, 2006"
                             (collection-file-path "endnotes.xml" "mox" "stone" "docx" "misc")
                             (opc-part-name-normalize/zip part-name))))

(define opc-word-settings-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.3, 2006"
                             (collection-file-path "settings.xml" "mox" "stone" "docx" "misc")
                             (opc-part-name-normalize/zip part-name))))

(define opc-word-websettings-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.13, 2006"
                             (collection-file-path "webSettings.xml" "mox" "stone" "docx" "misc")
                             (opc-part-name-normalize/zip part-name))))

(define opc-word-font-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-file-entry #:ascii? #true #:comment "Fundamentals 11.3.5, 2006"
                             (collection-file-path "font.xml" "mox" "stone" "docx" "misc")
                             (opc-part-name-normalize/zip part-name))))
