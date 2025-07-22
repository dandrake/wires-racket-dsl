#lang wires

// I am also allowing comments!

x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
123 -> x
456 -> y
y -> z
z AND 42 -> a
a -> b
1729 OR b -> cab

// test adding edges before bn defined, order shouldn't matter
bn RSHIFT 2 -> bnrshift
bn LSHIFT 2 -> bnlshift
5 AND bn -> bnand
5 OR bn -> bnor
NOT bn -> bnnot

1023 -> bn

SHOW d
SHOW e
SHOW f
SHOW g
SHOW x
SHOW y
SHOW z
SHOW a
