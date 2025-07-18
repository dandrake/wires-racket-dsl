#lang racket/base


(require racket/port)

;;; wires stuff


(define-syntax wires-operator%
                 ;; literals:
  (syntax-rules (AND OR LSHIFT RSHIFT NOT ->)
    [(wires-operator lhs AND rhs -> dest)
     (define (dest) (printf "bitwise AND ~s ~s to ~s" lhs rhs dest))]
    [(wires-operator NOT lhs -> dest)
     (define (dest) (printf "bitwise NOT of ~s to ~s" lhs dest))]
    [(wires-operator input -> dest)
     (define (dest) (printf "input ~s to ~s" input dest))]
    ))

#|

whoa this is like 80% of the work we need for this:

wires-scratch-pad.rkt> (wires-operator x AND y -> d)
wires-scratch-pad.rkt> (wires-operator 456 -> y)
wires-scratch-pad.rkt> (wires-operator 123 -> x)
wires-scratch-pad.rkt> (x)
input 123 to #<procedure:x>
wires-scratch-pad.rkt> (y)
input 456 to #<procedure:y>
wires-scratch-pad.rkt> (d)
bitwise AND #<procedure:x> #<procedure:y> to #<procedure:d>
wires-scratch-pad.rkt> (wires-operator NOT d -> e)
wires-scratch-pad.rkt> (e)
bitwise NOT of #<procedure:d> to #<procedure:e>
wires-scratch-pad.rkt>

|#


(define-syntax wires-operator
                 ;; literals:
  (syntax-rules (AND OR LSHIFT RSHIFT NOT -> //)
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


(define (readertest s)
  (for/list ([line (port->lines (open-input-string s)) ])
    ;; line of wires code ->
    ;; wrap in an sexp with our expander macro ->
    ;; make it into a port, because that's what 'read' needs ->
    ;; call read, which reads from a port and returns a datum ->
    ;; make that into a syntax object.
    (datum->syntax #f (read (open-input-string (format "(wires-operator ~a)" line))))))
