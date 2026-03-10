#lang typed/racket/base

(provide (all-defined-out))

(require digimon/dtrace)
(require digimon/scribble)

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/path)
  (require scribble/render)
  
  (require mox/village/scribble/shared/dtrace)
  (require (prefix-in docx: mox/village/scribble/docx))

  (define mox-docx-render
    (lambda [scrbl.doc dest.docx temp.docx dtrace-msg]
      (parameterize ([default-dtrace-sender dtrace-msg])
        (render #:dest-dir (path-only dest.docx)
                #:render-mixin docx:render-mixin
                #:style-file temp.docx
                (list scrbl.doc) (list dest.docx))))))

(require typed/racket/unsafe)

(unsafe-require/typed/provide
 (submod "." unsafe)
 [mox-docx-render (-> Part Path (Option Path) Scribble-Message Void)])
