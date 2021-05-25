#lang typed/racket/base

(provide (all-defined-out))

(require digimon/archive)

(require sgml/xexpr)

(require "../../package/partname.rkt")
(require "../../package/standards.rkt")
(require "../../package/xmlns.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define opc-word-document-markup-entry : (->* (String) (#:utc Integer) Archive-Entry)
  (lambda [part-name #:utc [ts #false]]
    (define story : Xexpr
      (list 'w:document (append `([xmlns:w . ,(assert (opc-xmlns 'Docx:W))]
                                  [xmlns:w10 . ,(assert (opc-xmlns 'Docx:W10))]
                                  [xmlns:w14 . ,(assert (opc-xmlns 'Docx:W14))]
                                  [xmlns:w15 . ,(assert (opc-xmlns 'Docx:W15))]
                                  [xmlns:w16 . ,(assert (opc-xmlns 'Docx:W16))]
                                  [xmlns:w16se . ,(assert (opc-xmlns 'Docx:W16se))]
                                  [xmlns:w16cid . ,(assert (opc-xmlns 'Docx:W16cid))]
                                  [xmlns:w16cex . ,(assert (opc-xmlns 'Docx:W16cex))]
                                  [xmlns:w16sdtdh . ,(assert (opc-xmlns 'Docx:W16sdtdh))]
                                  [xmlns:wp . ,(assert (opc-xmlns 'Docx:WP))]
                                  [xmlns:wp14 . ,(assert (opc-xmlns 'Docx:WP14))]
                                  [xmlns:wpc . ,(assert (opc-xmlns 'Docx:WPC))]
                                  [xmlns:wpi . ,(assert (opc-xmlns 'Docx:WPI))]
                                  [xmlns:wpg . ,(assert (opc-xmlns 'Docx:WPG))]
                                  [xmlns:wps . ,(assert (opc-xmlns 'Docx:WPS))]
                                  [xmlns:wne . ,(assert (opc-xmlns 'Docx:WNE))])
                                (mox-drawing-xmlns)
                                (mox-compatibility-xmlns))
            `((w:body ()
                      ((w:p ()
                            ((w:r ()
                                  ((w:t () ("Hello, World!")))))))))))
    
    (make-archive-ascii-entry #:utc-time ts #:comment "Primer 2.3, 2006"
                              (xexpr->bytes story #:prolog? #true)
                              (opc-part-name-normalize/zip part-name))))
