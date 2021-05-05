#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/list)

(require digimon/dtrace)

(require digimon/digitama/exec)
(require digimon/digitama/latex)

(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/native)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/spec)

(require digimon/digivice/wisemon/phony/typeset)

(require "../format.rkt")

(require/typed
 digimon/digitama/tamer
 [handbook-metainfo (-> Path-String String (Values String String))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-docx-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (for/list : Wisemon-Specification ([typesetting (in-list (find-digimon-typesettings info-ref #true))])
      (define-values (docx.scrbl maybe-name regexps) (values (car typesetting) (caddr typesetting) (cadddr typesetting)))
      (define mox.docx : Path (assert (tex-document-destination docx.scrbl #true #:extension #".docx")))
      
      (wisemon-spec mox.docx #:^ (filter file-exists? (tex-smart-dependencies docx.scrbl)) #:-
                    (define pwd : Path (assert (path-only mox.docx)))
                    (define ./docx : Path-For-Some-System (find-relative-path (current-directory) mox.docx))
                      
                    (dtrace-note "~a ~a: ~a" the-name (current-make-phony-goal) ./docx)
                    
                    (parameterize ([current-directory pwd]
                                   [current-namespace (make-base-namespace)]
                                   [exit-handler (λ _ (error the-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                                             the-name (current-make-phony-goal) ./docx))])
                      (eval '(require (prefix-in markdown: scribble/markdown-render) setup/xref scribble/render))
                      (eval `(define (markdown:render docx.scrbl #:dest-dir dest-dir)
                               (render #:dest-dir dest-dir #:render-mixin markdown:render-mixin
                                       (list (dynamic-require docx.scrbl 'doc)) (list ,mox.docx))))
                      (fg-recon-eval 'docx `(markdown:render ,docx.scrbl #:dest-dir ,(path-only mox.docx))))))))

(define make~docx : MOX-Render
  (lambda [digimon info-ref]
    (wisemon-make (make-native-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-docx-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-format : MOX-Format
  (devimon-make-format #:name 'docx #:render make~docx #:desc "MS Office Word Open XML Format"))
