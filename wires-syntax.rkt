;; wires-syntax.rkt

#lang racket/base

(provide
  wires-operator)

(define-syntax wires-operator
                 ;; literals:
  (syntax-rules (AND OR LSHIFT RSHIFT NOT -> // SHOW)
    [(wires-operator lhs AND rhs -> dest)
     (define (dest) (wires-and (lhs) (rhs)))]
    [(wires-operator lhs OR rhs -> dest)
     (define (dest) (wires-or (lhs) (rhs)))]
    [(wires-operator lhs LSHIFT n -> dest)
     (define (dest) (wires-lshift (lhs) n))]
    [(wires-operator lhs RSHIFT n -> dest)
     (define (dest) (wires-rshift (lhs) n))]
    [(wires-operator NOT lhs -> dest)
     (define (dest) (wires-not (lhs)))]
    [(wires-operator input -> dest)
     (define (dest) input)]
    [(wires-operator SHOW wire)
     (printf "wire ~s = ~s\n" (object-name wire) (wire))]
    [(wires-operator // comments ...)
     (void)]
    [(wires-operator)
     (void)]))

(define (wires-and x y)
  (bitwise-and x y))

(define (wires-or x y)
  (bitwise-ior x y))

(define (wires-not x)
  (bitwise-bit-field (bitwise-not x) 0 16))

(define (wires-lshift x n)
  (arithmetic-shift x n))

(define (wires-rshift x n)
  (arithmetic-shift x (- n)))
