#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)

(require digimon/dtrace)

(require digimon/digitama/exec)
(require digimon/digitama/latex)

(require (except-in digimon/digivice/wisemon/parameter the-name))
(require digimon/digivice/wisemon/native)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/spec)

(require digimon/digivice/wisemon/phony/typeset)

(require "../format.rkt")
(require "../parameter.rkt")
(require "../../../village/scribble/docx/metainfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-docx-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (for/list : Wisemon-Specification ([typesetting (in-list (find-digimon-typesettings info-ref))])
      (define-values (docx.scrbl maybe-name regexps) (values (tex-info-path typesetting) (tex-info-name typesetting) (tex-info-dependencies typesetting)))
      (define mox.docx : Path (assert (tex-document-destination docx.scrbl #true #:extension docx-suffix)))
      
      (wisemon-spec mox.docx #:^ (filter file-exists? (tex-smart-dependencies docx.scrbl)) #:-
                    (define pwd : Path (assert (path-only mox.docx)))
                    (define ./docx : Path-For-Some-System (find-relative-path (current-directory) mox.docx))
                      
                    (dtrace-note "~a ~a: ~a" the-name (current-make-phony-goal) ./docx)
                    
                    (parameterize ([current-directory pwd]
                                   [current-namespace (make-base-namespace)]
                                   [exit-handler (λ _ (error the-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                                             the-name (current-make-phony-goal) ./docx))])
                      (eval '(require (prefix-in docx: mox/village/scribble/docx) setup/xref scribble/render))
                      (eval `(define (docx:render docx.scrbl #:dest-dir dest-dir)
                               (render #:dest-dir dest-dir #:render-mixin docx:render-mixin
                                       (list (dynamic-require docx.scrbl 'doc)) (list ,mox.docx))))
                      (fg-recon-eval docx-render-mode `(docx:render ,docx.scrbl #:dest-dir ,(path-only mox.docx))))))))

(define make~docx : MOX-Render
  (lambda [digimon info-ref]
    (wisemon-make (make-native-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-docx-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-format : MOX-Format
  (devimon-make-format #:name 'docx #:render make~docx #:desc "MS Office Word Open XML Format"))
