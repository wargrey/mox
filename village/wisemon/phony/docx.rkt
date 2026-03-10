#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require digimon/dtrace)
(require digimon/scribble)

(require digimon/digivice/wisemon/spec)
(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/phony)
(require digimon/digivice/wisemon/phony/typeset)

(require digimon/digitama/tamer/stat)

(require mox/village/scribble/docx/metainfo)
(require mox/village/scribble/render)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-docx-specs : (-> (Option Path) Wisemon-Scribble->Specification)
  (lambda [template]
    (λ [engine scrbl.doc desc all-deps dtrace-msg]
      (define volume.scrbl (tex-desc-volume.scrbl desc))
      (define scrbl.docx (tex-desc-self.out desc))
      
      (wisemon-spec scrbl.docx #:^ all-deps #:-
                    (dtrace-info #:topic docx-render-mode "(~a ~a #:dest ~a #:dest-name ~a #:tag ~a)"
                                 (object-name mox-docx-render)
                                 volume.scrbl (path-only scrbl.docx) (file-name-from-path scrbl.docx)
                                 (cadar (part-tags scrbl.doc)))
                    
                    (mox-docx-render scrbl.doc scrbl.docx template dtrace-msg)
                    (handbook-display-metrics dtrace-msg 'note (handbook-stats scrbl.doc 'latex))))))

(define docx-specs : Wisemon-Scribble->Specification (make-docx-specs #false))

(define make~docx : Make-Info-Phony
  (lambda [digimon info-ref]
    (define typesettings : (Listof Tex-Info) (make-typeset-prepare digimon info-ref docx-render-mode))

    (when (pair? typesettings)
      (void (make-typeset typesettings (make-always-run) #false
                          docx-specs typeset-default-extension)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name docx-render-mode #:phony make~docx #:desc "Typeset publications in Docx Format from Scribble"))
