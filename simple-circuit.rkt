#lang wires-demo

// use wires-demo to test against BR's implementation

x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
123 -> x
456 -> y

// idea is that this'll get transformed into something like
