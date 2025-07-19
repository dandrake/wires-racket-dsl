# wires

a Racket-based domain-specific language for solving the "wires"
programming puzzle

This is my solution to / implementation of the [wires language /
programming puzzle from the great [Beautiful Racket
book](https://beautifulracket.com/wires/intro.html).

## motivation

Solving this problem by imagining the problem specification as a simple
domain-specific language, then writing a Racket macro and supporting
functions turned out to be very straightforward. Hooray for Racket!

But the setup and boilerplate to get it all working turns out to be
really tricky and finicky. Beautiful Racket ships a Racket package that
provides a lot of convenient syntactic sugar for making the reader,
parser, and expander for a new DSL. But I wanted to figure out how to do
this all in base, plain Racket.

It turns out that doing so is very finicky and not at all obvious.
There is a lot of brittle, obscure, finicky setup to make it all work.
