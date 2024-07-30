#lang typed/racket/base

(provide (all-defined-out))

(require digimon/filesystem)
(require digimon/dtrace)

(require digimon/digitama/exec)
(require digimon/digitama/latex)

(require (except-in digimon/digivice/wisemon/parameter the-name))
(require digimon/digivice/wisemon/ffi)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/spec)

(require digimon/digivice/wisemon/phony/typeset)

(require "../format.rkt")
(require "../parameter.rkt")
(require "../../../village/scribble/xlsx/metainfo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-xlsx-specs : (-> Info-Ref Wisemon-Specification)
  (lambda [info-ref]
    (define typesettings : (Listof Tex-Info) (find-digimon-typesettings info-ref xlsx-list-renderers))
    
    (for/list : Wisemon-Specification ([typesetting (in-list typesettings)])
      (define-values (xlsx.scrbl maybe-name regexps) (values (tex-info-path typesetting) (tex-info-name typesetting) (tex-info-dependencies typesetting)))
      (define mox.xlsx : Path (assert (tex-document-destination xlsx.scrbl #true #:extension xlsx-suffix)))
      
      (wisemon-spec mox.xlsx #:^ (filter file-exists? (tex-smart-dependencies xlsx.scrbl)) #:-
                    (define pwd : Path (assert (path-only mox.xlsx)))
                    (define ./xlsx : Path-For-Some-System (find-relative-path (current-directory) mox.xlsx))
                    
                    (dtrace-note "~a ~a: ~a" the-name (current-make-phony-goal) ./xlsx)
                    
                    (parameterize ([current-directory pwd]
                                   [current-namespace (make-base-namespace)]
                                   [exit-handler (λ _ (error the-name "~a ~a: [fatal] ~a needs a proper `exit-handler`!"
                                                             the-name (current-make-phony-goal) ./xlsx))])
                      (eval '(require (prefix-in xlsx: mox/village/scribble/xlsx) setup/xref scribble/render))
                      (eval `(define (xlsx:render xlsx.scrbl #:dest-dir dest-dir)
                               (render #:dest-dir dest-dir #:render-mixin xlsx:render-mixin
                                       (list (dynamic-require xlsx.scrbl 'doc)) (list ,mox.xlsx))))
                      (fg-recon-eval xlsx-render-mode `(xlsx:render ,xlsx.scrbl #:dest-dir ,(path-only mox.xlsx))))))))

(define make~xlsx : MOX-Render
  (lambda [digimon info-ref]
    (wisemon-make (make-ffi-library-specs info-ref))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-xlsx-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xlsx-list-renderers : (-> (Listof Symbol))
  ; for satisfying the `find-digimon-tyesettings`, which is designed for Latex and docx
  (lambda []
    '(excel Excel xlsx xls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define docx-format : MOX-Format
  (devimon-make-format #:name 'xlsx #:render make~xlsx #:desc "MS Office Excel Open XML Format"))
