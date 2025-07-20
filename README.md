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
There is a lot of brittle, obscure, finicky setup to make it all work:
see [my thread on the Racket discourse
forum](https://racket.discourse.group/t/confused-about-setup-for-reader-and-expander-for-simple-dsl/3859).

## a plea to the Racket team: make this easier

Racket is pitched as a "language-oriented language" that is especially
well-suited to making new languages and DSLs. And it is!

But I think the Racket community should make it as easy as possible for
people to show up with a DSL and a basic working knowledge of
Racket/Scheme/Lisp, and go from there to a working, usable DSL.

For this project, the DSL was given to me, and it was straightforward
and fun to figure out how to convert `wires` syntax into Racket code
that solved the original wires puzzle. But the rest of it was hard and
not fun. It had the flavor of tedious boilerplate and brittle
configuration -- *this* file needs to be in *that* directory; another
thing needs to provide a very certain kind of thing to appease Racket's
module system, and so on.

I wish that was easier and more straightforward. Or at least
better-documented.

I do hope this repo serves as an example that others can use.

I think the Racket team suffers a bit from the "curse of expertise":
they are so deeply familiar with every detail and subtlety of Racket's
module system, of how `raco` and packages work, and how `#lang foo`
works, that it's easy for them to do it -- but they aren't aware of how
hard it is for a newbie like myself to get it all working.

### a metaphor: the driving route to a destination

Think about driving to a destination in a city downtown with
lots of confusing one-way streets, many turns, along a route on which
it's easy to get lost or take a wrong turn.

If you're familiar with the route and have driven it a few times, it's
easy and straightforward to follow it. And you likely know about some
alternatives that work just as well.

But if you're not, it's confusing and difficult, and if you take one
wrong turn, you get into trouble and don't know how to get back on
track.

That destination, here, is "a working `#lang wires` setup". The Racket
developers are intimately familiar with the route. And it was, I think,
too hard for me to learn it.

What I hope the Racket community does, in terms of this metaphor, is to
make this destination as easy as possible to get to. Either move it to
somewhere in the city that's easier to navigate to (that is, add or
modify core Racket stuff), or perhaps just add a bunch of signs (better
documentation, easy-to-follow tutorials, examples, and so on.)
