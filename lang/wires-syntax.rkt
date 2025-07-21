;; wires-syntax.rkt

#lang racket/base

(provide
  wires-operator)

(require (for-syntax syntax/parse racket/base))

(begin-for-syntax
  ;; here you can combine multiple patterns into one,
  ;; if they share certain properties or should be treated similarly
  (define-syntax-class wires-atom
    #:attributes (expand)
    (pattern id:id      #:with expand #'(id))
    (pattern num:number #:with expand #'num)))

(define-syntax (wires-operator stx)
  (syntax-parse stx
    #:datum-literals (AND OR LSHIFT RSHIFT NOT -> // SHOW)

    [(wires-operator lhs:wires-atom AND rhs:wires-atom -> dest:id)
     #'(define (dest)
         (wires-and lhs.expand rhs.expand))]

    [(wires-operator lhs:wires-atom OR rhs:wires-atom -> dest:id)
     #'(define (dest)
         (wires-or lhs.expand rhs.expand))]

    [(wires-operator lhs:wires-atom LSHIFT n:number -> dest:id)
     #'(define (dest)
         (wires-lshift lhs.expand n))]

    [(wires-operator lhs:wires-atom RSHIFT n:number -> dest:id)
     #'(define (dest)
         (wires-rshift lhs.expand n))]

    [(wires-operator NOT lhs:wires-atom -> dest:id)
     #'(define (dest)
         (wires-not lhs.expand))]

    [(wires-operator input:expr -> dest:id)
     #'(define (dest) input)]

    [(wires-operator SHOW wire)
     #'(printf "wire ~s = ~s\n" (object-name wire) (eval-rec wire))]

    [(wires-operator // comments ...)
     #'(void)]

    [(wires-operator)
     #'(void)]))

#;(define-syntax wires-operator
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
     (define (dest) (input))]
    [(wires-operator SHOW wire)
     (printf "wire ~s = ~s\n" (object-name wire) (eval-rec wire)
             )]
    [(wires-operator // comments ...)
     (void)]
    [(wires-operator)
     (void)]))

(require racket/set)

;; recursive evaluate: if it's a number, return that; otherwise, it's
;; a function and we ask eval-rec to, well, recursively evaluate the
;; function's output.
(define (eval-rec x [seen (mutable-set)])
  (printf "eval-rec ~s, ~s\n" x seen)
  (if (set-member? seen x)
      (raise-arguments-error 'x "have seen this before")
      (begin
        (set-add! seen x)
        (if (number? x) x (eval-rec (x) seen))
)))

(define (wires-and x y)
  (bitwise-and (eval-rec x) (eval-rec y)))

(define (wires-or x y)
  (bitwise-ior (eval-rec x) (eval-rec y)))


(define (wires-not x)
  (bitwise-bit-field (bitwise-not (eval-rec x)) 0 16))

(define (wires-lshift x n)
  (arithmetic-shift (eval-rec x) n))

(define (wires-rshift x n)
  (arithmetic-shift (eval-rec x) (- n)))
