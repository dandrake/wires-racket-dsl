#lang wires

// executing this should fail and report those wires in a cycle:


a -> b
b -> c
c -> a

1 -> d
d -> e
