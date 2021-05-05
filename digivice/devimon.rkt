#lang typed/racket/base

(provide main)

(require racket/path)

(require digimon/digitama/collection)
(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/racket)

(require digimon/dtrace)
(require digimon/cmdopt)
(require digimon/debug)
(require digimon/system)
(require digimon/port)

(require "devimon/format.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option devimon-flags #: Devimon-Flags
  #:program 'devimon
  #:args formats丨file-path

  #:once-each
  [[(#\B always-make)         #:=> make-always-run                       "Unconditionally re-render all documents"]
   [(#\i ignore-errors)       #:=> (λ _ (make-errno 0))                  "Do not tell shell there are errors"]
   [(#\n dry-run recon)       #:=> make-dry-run                          "Don't actually run any commands; just print them [Except *.rkt]"]
   [(#\s slient quiet)        #:=> (λ _ (current-output-port /dev/null)) "Just render and only display errors"]
   [(#\t touch)               #:=> make-just-touch                       "Touch targets instead of re-rendering them if existed"]
   [(#\d debug)               #:=> make-trace-log                        "Print lots of debug information"]
   [(#\v verbose)             #:=> make-set-verbose!                     "Render with verbose messages"]
   [(#\k keep-going)          #:=> make-keep-going                       "Keep going when some targets cannot be rendered"]
   [(#\j jobs)                #:=> cmdopt-string->byte n #: Byte         ["Allow ~1 jobs at once [0 for default: ~a]" (parallel-workers)]]]
  
  #:multi
  [[(#\W new-file assume-new) #:=> cmdopt-string->path FILE #: Path      "Consider ~1 to be infinitely new"]
   [(#\o old-file assume-old) #:=> cmdopt-string->path FILE #: Path      "Consider ~1 to be infinitely old and do not remaking them"]])

(define devimon-display-help : (->* () ((Option Byte)) Void)
  (lambda [[retcode 0]]
    (define formats : (Immutable-HashTable Symbol MOX-Format) (mox-list-formats))
    (define format-helps : (Listof String)
      (for/list ([p (in-list '(docx xlsx pptx))]
                 #:when (hash-has-key? formats p))
        (format "    ~a : ~a" p (mox-format-description (hash-ref formats p)))))
    
    (display-devimon-flags #:more-ps (cons "  where <format> is one of" format-helps)
                           #:exit retcode)))

(define devimon-format-partition : (-> (Listof String) (Values (Pairof MOX-Format (Listof MOX-Format)) (Listof Path)))
  (lambda [goals]
    (define-values (seinohp slaer)
      (for/fold ([seinohp : (Listof MOX-Format) null]
                 [slaer : (Listof Path) null])
                ([g (in-list goals)])
        (cond [(path-get-extension g) (values seinohp (cons (simple-form-path g) slaer))]
              [(mox-format-ref (string->symbol g)) => (λ [[p : MOX-Format]] (values (cons p seinohp) slaer))]
              [else (values seinohp (cons (simple-form-path g) slaer))])))
    (let ([phonies (reverse seinohp)])
      (values (if (pair? phonies) phonies (list (assert (mox-format-ref 'all))))
              (reverse slaer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define render-digimon : (-> (U Pkg-Info (Pairof Info-Ref (Listof Pkg-Info))) (Listof Path) (Pairof MOX-Format (Listof MOX-Format)) Byte)
  (lambda [info scrbls formats]
    (cond [(pair? info) (for/fold ([retcode : Byte 0]) ([subinfo (in-list (cdr info))]) (render-digimon subinfo scrbls formats))]
          [else (let ([zone (pkg-info-zone info)]
                      [info-ref (pkg-info-ref info)]
                      [tracer (thread (make-racket-log-trace))])
                  (parameterize ([current-make-real-targets scrbls]
                                 [current-digimon (pkg-info-name info)]
                                 [current-free-zone zone]
                                 [current-directory zone])
                    (dtrace-notice "Enter Digimon Zone: ~a" (current-digimon))
                    (begin0 (for/fold ([retcode : Byte 0])
                                      ([phony (in-list formats)])
                              (parameterize ([current-make-phony-goal (mox-format-name phony)]
                                             [current-custodian (make-custodian)])
                                (begin0 (with-handlers ([exn:break? (λ [[e : exn:break]] (newline) 130)]
                                                        [exn:fail? (λ [[e : exn]] (dtrace-exception e #:level 'fatal #:brief? (not (make-verbose))) (make-errno))])
                                          ((mox-format-render phony) (current-digimon) info-ref)
                                          retcode)
                                        (custodian-shutdown-all (current-custodian)))))

                            (dtrace-datum-notice eof "Leave Digimon Zone: ~a" (current-digimon))
                            (thread-wait tracer))))])))

(define main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (make-restore-options!)
    (define-values (options λargv) (parse-devimon-flags argument-list))

    (when (devimon-flags-help? options)
      (devimon-display-help))

    (let ([jobs (devimon-flags-jobs options)])
      (when (and jobs (> jobs 0))
        (parallel-workers jobs)))

    (make-assume-oldfiles (devimon-flags-old-file options))
    (make-assume-newfiles (devimon-flags-new-file options))

    (parameterize ([current-logger /dev/dtrace])
      (define digimons (collection-info))
      (if (not digimons)
          (let ([retcode (make-errno)])
            (call-with-dtrace (λ [] (dtrace-fatal "fatal: not in a digimon zone")))
            (exit retcode))
          (let-values ([(formats scrbls) (devimon-format-partition (λargv))])
            (exit (time* (render-digimon digimons scrbls formats))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main (current-command-line-arguments))
