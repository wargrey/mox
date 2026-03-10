#lang typed/racket/base

(provide main)

(require racket/pretty)
(require racket/case)
(require racket/path)

(require digimon/dtrace)
(require digimon/debug)
(require digimon/custodian)

(require digimon/digivice/wisemon/display)
(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/phony/typeset)
(require digimon/digivice/wizarmon/echo)
(require digimon/digivice/wizarmon/cmdopt)

(require digimon/digitama/exec)
(require digimon/digitama/system)
(require digimon/digitama/collection)
(require digimon/digitama/minimal/port)
(require digimon/digitama/tamer/selector)

(require mox/village/wisemon/phony/docx)

(require "devimon/parameter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option devimon-flags #: Devimon-Flags
  #:program (short-program+command-name)
  #:args [file . args]

  #:usage-help "translate Scribble into MS Office Word document"
  #:once-each
  [[(#\w print-columns) #:=> cmdopt-string+>index columns    #: Index                   ["use ~1 as the default width for pretty printing (default: ~a)"
                                                                                         the-print-width]]

   [(#\B always-remake) #:=> devimon-remake                                             "unconditionally remake the target"]
   [(#\s slient quiet)  #:=> devimon-silent                                             "suppress the standard output"]
   [(#\d debug)         #:=> devimon-debug                                              "run with debug information"]
   [(#\v verbose)       #:=> devimon-verbose                                            "run with verbose messages"]
   [(no-preview)        #:=> devimon-skip-view                                          "do not open the file after generating"]

   [(#\f flatten)                                                                       "perform a granular offprinting"]
   [(#\S strip)                                                                         "remove prefaces and bonus appendices"]

   [(#\F format)        #:=> cmdopt-string->symbol format    #: Symbol                  "specify the target format to ~1 (default: docx)"]
   [(#\R reference)     #:=> cmdopt-string->path template    #: Path                    "use ~1 as the template document"]]
  
  #:multi
  [[(chapter seq)       #:=> cmdopt-string->chapter-index id #: Handbook-Chapter-Index  "build the part or chapter whose number is ~1"]])

(define make-mox-specs : (-> Symbol (-> (Option Path) Wisemon-Scribble->Specification))
  (lambda [target-format]
    (case/eq target-format
       [(docx) make-docx-specs]
       [else (raise (make-exn:fail:unsupported (format "~a: don't know how to generate ~a file"
                                                 (the-cmd-name) target-format)
                                               (continuation-marks #false)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mox-typeset : (-> Path Devimon-Flags Byte)
  (lambda [src.scrbl options]
    (define tformat (or (devimon-flags-format options) 'docx))
    (define template (devimon-flags-reference options))

    (dtrace-notice #:topic (the-cmd-name) "source: ~a" src.scrbl)

    (when (devimon-flags-format options)
      (dtrace-notice #:topic (the-cmd-name) "format: ~a" tformat))
    
    (when (devimon-flags-reference options)
      (dtrace-notice #:topic (the-cmd-name) "reference: ~a" template))
    
    (call-in-nested-custodian
     (λ [] (parameterize ([current-error-port (open-output-dtrace 'error)]
                          [current-output-port (if (devimon-silent) /dev/null (current-output-port))])
             (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)]
                             [exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:level 'fatal #:brief? #false) (devimon-errno))])
               (if (pair? (devimon-flags-chapter options))
                   (let ([selector (make-user-specified-selector (devimon-flags-chapter options)
                                                                 (devimon-flags-flatten options)
                                                                 (devimon-flags-strip options))])
                     (parameterize ([current-user-specified-selector selector]
                                    [current-user-request-no-volume? #true])
                       (do-typeset src.scrbl tformat template)))
                   (do-typeset src.scrbl tformat template))
               0))))))

(define do-typeset : (-> Path Symbol (Option Path) Void)
  (lambda [path.scrbl target-format template]
    (define maybe-info : (Option Pkg-Info)
      (single-collection-info #:bootstrap? #true
                              (or (path-only path.scrbl)
                                  (current-directory))))

    (parameterize ([make-verbose (devimon-verbose)]
                   [make-trace-log (devimon-debug)]
                   [current-make-phony-goal target-format]
                   [current-make-real-targets (list path.scrbl)]
                   [current-digimon (if maybe-info (pkg-info-name maybe-info) (current-digimon))]
                   [current-directory (if maybe-info (pkg-info-zone maybe-info) (assert (path-only path.scrbl)))])
      (define all-typesettings : (Listof Tex-Info)
        (cond [(not maybe-info) (make-typeset-prepare "" #false target-format)]
              [else (make-typeset-prepare (pkg-info-name maybe-info)
                                          (pkg-info-ref maybe-info)
                                          target-format)]))

      (when (pair? all-typesettings)
        (define targets : (Listof Path)
          (make-typeset all-typesettings (devimon-remake) #false
                        ((make-mox-specs target-format) template)
                        typeset-default-extension))

        (unless (devimon-skip-view)
          (let try-open ([targets : (Listof Path) targets])
            (when (pair? targets)
              (if (regexp-match? #px"\\.(docx|dotx|xlsx|xltx|pptx|potx)$" (car targets))
                  (fg-recon-open-file 'exec (car targets))
                  (try-open (cdr targets))))))))))
  
(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (devimon-restore-options!)
    (define-values (options λargv) (parse-devimon-flags argument-list))

    (when (devimon-flags-help? options)
      (display-devimon-flags #:exit 0))

    (define-values (target argv) (λargv))
    
    (let ([tracer (thread (make-wizarmon-log-trace (devimon-debug) (devimon-verbose)))])
      (parameterize ([current-logger /dev/dtrace]
                     [pretty-print-columns (or (devimon-flags-print-columns options) the-print-width)]
                     [current-command-line-arguments (list->vector argv)])  
        (exit (time* (begin0 (mox-typeset (cmdopt-string->path (the-cmd-name) target) options)
                             (dtrace-sentry-notice #:end? #true eof)
                             (thread-wait tracer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
