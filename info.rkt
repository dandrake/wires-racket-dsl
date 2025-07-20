#lang info
(define collection "wires")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/wires.scrbl" ())))
(define pkg-desc "a DSL for solving the 'wires' programming puzzle")
(define version "0.0")
(define pkg-authors '("Dan Drake (https://github.com/dandrake/)"))
(define license '(Apache-2.0 OR MIT))
