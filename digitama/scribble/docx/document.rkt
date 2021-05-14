#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require "../package/partname.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-document-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (make-archive-ascii-entry #:utc-time ts #:comment "OpenPackagingConventions 9.1.2.2, 2006"
                              "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n
<w:document xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:body><w:p/></w:body></w:document>"
                              (opc-part-name-normalize/zip part-name))))
