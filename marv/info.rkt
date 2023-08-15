#lang info
(define collection "marv")
(define deps '("graph-lib"
               "http-easy-lib"
               "rackunit-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/marv.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(Kevin Haines))
(define license '(Apache-2.0 OR MIT))
