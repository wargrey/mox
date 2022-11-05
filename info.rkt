#lang info

(define collection 'use-pkg-name)
(define pkg-authors '(wargrey))

(define pkg-desc "MOX: Open XML for Microsoft Office")
(define version "1.0")

(define deps '("base" "digimon" "w3s" "typed-racket-lib" "typed-racket-more" "scribble-lib"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "digimon" "graphics"))

(define raco-commands '(["devimon" mox/digivice/devimon "translate Scribble into MS Office documents" #false]))

(define scribblings '(["tamer/mox.scrbl" (main-doc multi-page) (parsing-library)]))

(define typesettings '(["tamer/mox.scrbl"]))
