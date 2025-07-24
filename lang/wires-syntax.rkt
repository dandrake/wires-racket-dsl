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

(define dependencies (directed-graph empty))
;; above we provide this, so you can get it in the REPL. The following
;; is nice for visualization.

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


#|

While flailing around, not yet understanding why my evaluation was so
slow and why memoization wasn't working, on my ideas was to handle the
memoization by iteratively evaluating wires whose distance from the root
of the depenedency graph (a numerical constant input) increases. In
principle, then, you can avoid the combinatiorial explosion in the
recursive evaluation -- see above.

In the end, though, I figured out the correct way to handle the
memoization. So none of this is necessary any more, but gosh I like my
nice graph algorithms and so on...

(define known-deps (make-hash))

;; Given a wire w, this outputs a set of all other wires that w depends
;; on -- those wires whose values can influence w's value. I used this
;; while trying to understand why my initial approach was so slow and
;; why my initial efforts to memoize the wire evaluation didn't work.
;;
;; This function, OTOH, memoizes very easily using the above known-deps
;; hash table; it doesn't need a macro helper because it has access to
;; the name of the thing it should memoize.
(define (transitive-dependencies wire)
  (cond
    [(hash-has-key? known-deps wire)
     (hash-ref known-deps wire)]
    [else
     (let ([result (let ([inputs (remove* '(numerical-signal)
                                          (sequence->list
                                           (in-neighbors dependencies wire)))])
                     (cond
                       [(empty? inputs)
                        (set)]
                       [else
                        (for/fold ([acc (set)])
                                  ([input (in-list inputs)])
                          (set-union acc
                                     (set-union (transitive-dependencies input)
                                                (set input))))]))])
       (hash-set! known-deps wire result)
       result)]))



;; The idea here is to iteratively build up known values for the hash
;; set, so that as we go along, eval-rec can use the hash table to do
;; lookups, eliminating the combinatorial explosion in number of paths
;; to the root.
;;
;; This function works, but there's a problem: the bfs returns distances
;; as the *shortest* path from the root to a node. But there can be
;; multiple paths from a wire to the numerical-signal root. So you can
;; have a wire that takes two inputs: a numerical signal (so its
;; distance is 1) but in the other branch, it may be distance 10 from
;; the root, with many little cycles along the way. So when eval-rec
;; sees that on the second iteration, it has to go down the distance-10
;; branch, and there's nothing in the hash table there yet.
(define (build-known-values)
  (define nodes (reverse (tsort dependencies)))
  ;; need the arrows pointing the other way for the neighbors function
  ;; give us what we want:
  (define deps (transpose dependencies))
  ;; hash table mapping nodes to the length of the *longest* path to the
  ;; root. Start with 0 for the root:
  (define distances (make-hash (for/list ([node nodes]) (cons node #f))))
  (hash-set! distances 'numerical-signal 0)

  ;; iterate over a topological sort of the nodes and for each node, set
  ;; the distance of its neighbors -- those nodes one step farther from
  ;; the root -- to one more than the node's distance. The sort
  ;; guarantees that the last time a node's distance is updated, it will
  ;; represent the longest path length.
  (for ([node nodes]
        #:do [(define new-dist (add1 (hash-ref distances node)))])
    (for ([nbr (get-neighbors deps node)])
      (hash-set! distances nbr new-dist)))

  ;; now iterate over the inverse of the distances to successively
  ;; populate node values in order of increasing distance
  (define dists (invert-hash-table distances))
  (for ([dist (rest (hash-keys dists))])
    (for ([node (hash-ref dists dist )])
      (printf "  would eval-rec ~s to ~s\n" node (eval-rec (eval node)))
      (hash-set! known-wire-values node (eval-rec (eval node))))))

;; invert a hash table: the input maps keys to values; the output's keys
;; are the values from the input, and its values are lists of keys in
;; the input corresponding to the value.
(define (invert-hash-table ht)
  (define values-to-key-list (make-immutable-hash
                  (for/list ([val (set->list (apply set (hash-values ht)))])
                    (cons val empty))))
  (for/fold ([acc values-to-key-list])
            ([pair (hash->list ht)])
    (let* ([val (cdr pair)] ;; 'second' only works on lists, this is a pair
           [key (car pair)] ;; similar
           [keylist (hash-ref acc val)])
      (hash-set acc val (cons key keylist)))))


|#
