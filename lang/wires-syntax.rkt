;; wires-syntax.rkt

#lang racket/base

(provide
 wires-operator
 dependencies
 wires-show-graph
)

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
    #:datum-literals (AND OR LSHIFT RSHIFT NOT -> // SHOW GRAPH)
    [(wires-operator lhs:wires-atom AND rhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (update-dependencies! 'rhs 'dest)
         (define (dest)
           (wires-and lhs.expand rhs.expand)))]
    [(wires-operator lhs:wires-atom OR rhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (update-dependencies! 'rhs 'dest)
         (define (dest)
           (wires-or lhs.expand rhs.expand )))]
    [(wires-operator lhs:wires-atom LSHIFT n:number -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (wires-lshift lhs.expand n)))]
    [(wires-operator lhs:wires-atom RSHIFT n:number -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (wires-rshift lhs.expand n)))]
    [(wires-operator NOT lhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (wires-not lhs.expand)))]
    [(wires-operator input:expr -> dest:id)
     #'(begin
         (update-dependencies! 'input 'dest)
         (define (dest) input))]
    [(wires-operator SHOW wire)
     #'(wires-show wire )]
    [(wires-operator GRAPH)
     #'(wires-show-graph)]
    [(wires-operator // comments ...)
     #'(void)]
    [(wires-operator)
     #'(void)]))

(require racket/set)

;; recursive evaluate: if it's a number, return that; otherwise, it's
;; a function and we ask eval-rec to, well, recursively evaluate the
;; function's output.
#;(define (eval-rec x [seen (mutable-set)])
  (printf "eval-rec ~s, ~s\n" x seen)
  (if (set-member? seen x)
      (raise-arguments-error 'x "have seen this before")
      (begin
        (set-add! seen x)
        (if (number? x) x (eval-rec (x) seen))
)))

(define (eval-rec wire)
  (cond
    [(number? wire) wire]
    [else
     (eprintf "eval-rec ~a\n" (object-name wire))
     (eval-rec (wire))]))

(require graph racket/list)
(define dependencies (directed-graph empty))
;; above we provide this, so you can get it in the REPL. The following
;; is nice for visualization.

;; QUESTION TODO should the number of bits for lshift and rshift be
;; considered a numerical signal input?

(define edgelist empty)

#;(define (write-viz g fn)
  (call-with-output-file fn
    #:exists 'truncate
    (lambda (out)
      (display (graphviz g) out))))



(define (add-edge-to-dag src dest)
  (cond
    [(number? src)
     (add-directed-edge! dependencies 'numerical-signal dest)]
    [else
     (add-directed-edge! dependencies src dest)]))

(define (update-dependencies! src dest)
  (add-edge-to-dag src dest))

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

(define (wires-show wire)

  (printf "wire ~s = ~s\n" (object-name wire) (eval-rec wire)))

(define (wires-show-graph)
  (printf "deps: ~a\n" (get-edges dependencies)))
