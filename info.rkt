#lang info

(define collection 'use-pkg-name)
(define pkg-authors '(wargrey))

(define pkg-desc "MOX: Open XML for Microsoft Office")
(define version "1.0")

(define scribblings '(["tamer/mox.scrbl" (main-doc multi-page) (parsing-library)]))

(define deps '("base" "graphics" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "digimon" "graphics"))
