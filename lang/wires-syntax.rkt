;; wires-syntax.rkt

#lang racket/base

(provide
 (all-defined-out))

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

;; the dependency graph can have multiple paths from a wire to the
;; ultimate source numerical signals, and with naive recursion you get a
;; combinatorial explosion with the number of call paths -- so we need
;; to memoize.


(define known-wire-values (make-hash))

(define (eval-rec wire)
  (define known-value (hash-ref known-wire-values (object-name wire) #f))
  (cond
    [known-value
     known-value]
    [(number? wire)
     wire]
    [else
     (eval-rec (wire))]))

(require graph racket/list)
(provide (all-from-out graph))

(define dependencies (directed-graph empty))
;; above we provide this, so you can get it in the REPL. The following
;; is nice for visualization.

;; QUESTION TODO should the number of bits for lshift and rshift be
;; considered a numerical signal input?

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

(define (wires-show-graph)
  (printf "deps: ~a\n" (get-edges dependencies)))

(require racket/sequence racket/set)

(define known-deps (make-hash))

;; Given a wire w, this outputs a set of all other wires that w depends
;; on -- those wires whose values can influence w's value.
;;
;; This function memoizes very easily using the above known-deps hash
;; table.
(define (transitive-dependencies wire)
  (cond
    [(hash-has-key? known-deps wire)
     (eprintf "known-deps ~s\n" wire)
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
  (define-values (distances _) (bfs (transpose dependencies) 'numerical-signal))
  (define dists-to-nodes (invert-hash-table distances))
  (for ([dist (in-naturals 1)]
        #:do [(define wires (hash-ref dists-to-nodes dist #f))]
        #:break (not wires))
    (for-each (lambda (wire)
                (hash-set! known-wire-values wire (eval-rec (eval wire))))
              wires)))

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
