;; wires-syntax.rkt

#lang racket/base

(provide
 (all-defined-out))

(require (for-syntax syntax/parse racket/base))
(require graph racket/list)
(provide (all-from-out graph))
(require rackunit)

(begin-for-syntax
  ;; here you can combine multiple patterns into one,
  ;; if they share certain properties or should be treated similarly
  (define-syntax-class wires-atom
    #:attributes (expand)
    (pattern id:id      #:with expand #'(id))
    (pattern num:number #:with expand #'num)))

(define-syntax (wires-operator stx)
  (syntax-parse stx
    #:datum-literals (AND OR LSHIFT RSHIFT NOT -> // SHOW ASSERT)
    [(wires-operator lhs:wires-atom AND rhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (update-dependencies! 'rhs 'dest)
         (define (dest)
           (with-memoization
             (wires-and lhs.expand rhs.expand)
             dest)))]
    [(wires-operator lhs:wires-atom OR rhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (update-dependencies! 'rhs 'dest)
         (define (dest)
           (with-memoization
            (wires-or lhs.expand rhs.expand)
            dest
            )))]
    [(wires-operator lhs:wires-atom LSHIFT n:number -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (with-memoization
           (wires-lshift lhs.expand n)
           dest
           )))]
    [(wires-operator lhs:wires-atom RSHIFT n:number -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (with-memoization
             (wires-rshift lhs.expand n)
             dest
             )))]
    [(wires-operator NOT lhs:wires-atom -> dest:id)
     #'(begin
         (update-dependencies! 'lhs 'dest)
         (define (dest)
           (with-memoization
             (wires-not lhs.expand)
             dest
             )))]
    [(wires-operator input:expr -> dest:id)
     #'(begin
         (update-dependencies! 'input 'dest)
         (define (dest)
           (with-memoization
             input
             dest)))]
    [(wires-operator SHOW wire)
     #'(wires-show wire)]
    [(wires-operator ASSERT wire output)
     #'(wires-assert wire output)]
    [(wires-operator // comments ...)
     #'(void)]
    [(wires-operator)
     #'(void)]))

;; helper macro: we need something that handles memoization at macro
;; expansion time, because of eager evaluation: the "wires-*" operator
;; functions evaluate their arguments, and if the definition of those
;; arguments involves other wire-* operator functions, *those*
;; evaluate..and so on, and the result is you don't have access the
;; *name* of the thing you're trying to memoize.
;;
;; Controlling when and how things get evaluated is one of the central
;; things you *do* with macros. So we handle it this way.
(define-syntax-rule (with-memoization input dest)
  (let ([hash-val (hash-ref known-wire-values dest #f)])
    (or hash-val
        (let ([output (eval-rec input)])
          (hash-set! known-wire-values dest output)
          output))))

(define known-wire-values (make-hash))

;; recursive evaluate: if it's a number, return that; otherwise, it's
;; a function and we ask eval-rec to, well, recursively evaluate the
;; function's output.

;; the dependency graph can have multiple paths from a wire to the
;; ultimate source numerical signals, and with naive recursion you get a
;; combinatorial explosion with the number of call paths -- so we need
;; to memoize. Here, we just consult the hash table and don't update it
;; -- see above.
(define (eval-rec wire)
  (define known-value (hash-ref known-wire-values (object-name wire) #f))
  (cond
    [known-value
     known-value]
    [(number? wire)
     wire]
    [else
      (eval-rec (wire)) ]))

;; A couple functions related to computing and working with the
;; dependency graph for a wires program. These are all provided, so you
;; can use them from the REPL.

(define dependencies (directed-graph empty))

(define (write-viz [fn "dependencies.dot"])
  (call-with-output-file fn
    #:exists 'truncate
    (lambda (out)
      (display (graphviz dependencies) out))))

(define (add-edge-to-dag src dest)
  (cond
    [(number? src)
     (add-directed-edge! dependencies dest 'numerical-signal)]
    [else
     (add-directed-edge! dependencies dest src)]))

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

(define (wires-assert wire value)
  (check-equal? (eval-rec wire) value (format "wire ~s" (object-name wire))))

(define (wires-dag?)
  (unless (dag? dependencies)
    (error "Provided input has circular dependencies among these wires:" (find-wires-in-cycles))))

(require threading racket/set)
(define (find-wires-in-cycles)
  ;; hash-filter-values isn't available yet in the version of Racket on
  ;; my system, so fake it by turning filtered-out entries into (#f #f)
  ;; and dropping that.

  ;; filter out pairs with numerical signal input, and those wire pairs
  ;; for which there's no path between them:
  (define (valid-kvp? k v) (and
                            v
                            (not (member 'numerical-signal k))
                            (not (equal? (car k) (cadr k)))))
  (define (filter-kvp k v) (if (valid-kvp? k v)
                               (values k v)
                               (values #f #f)))
  (define pairs (~>
     dependencies
     transitive-closure
     (hash-map/copy _
                    filter-kvp
                    #:kind 'immutable)
     (hash-remove _ #f)
     hash-keys
     ))
  (define (reverse-not-a-key? k)
    (let ([first (car k)]
          [second (cadr k)])
      (not (member pairs '(second first)))))
  (~> pairs
      (filter reverse-not-a-key? _)
     flatten
     (apply set _)))



#|

While flailing around, not yet understanding why my evaluation was so
slow and why memoization wasn't working, on my ideas was to handle the
memoization by iteratively evaluating wires whose distance from the root
of the depenedency graph (a numerical constant input) increases. In
principle, then, you can avoid the combinatiorial explosion in the
recursive evaluation -- see above.

In the end, though, I figured out the correct way to handle the
memoization. So none of this is necessary any more. See the
puzzle-testing branch for that code.

|#
