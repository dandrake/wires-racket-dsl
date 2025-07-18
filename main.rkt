#lang racket

(require racket/port)

(module reader racket
  (require racket/port)
  (provide (rename-out [wires-read read]
                       [wires-read-syntax read-syntax]))

  (define (wires-read port)
    (syntax->datum (wires-read-syntax #f port)))

  (define (wires-read-syntax path port)
    (define src-lines (port->lines port))
    (define src-datums
      (for/list ([line src-lines])
        ;; line of wires code ->
        ;; wrap in an sexp with our expander macro ->
        ;; make it into a port, because that's what 'read' needs ->
        ;; call read, which reads from a port and returns a datum
        (read (open-input-string (format "(wires-operator ~a)" line)))))
    (define module-datum `(module any-name-here wires
                            ,@src-datums))
    (datum->syntax #f module-datum)))


;; (require "wires-scratch-pad.rkt")
;; (provide (all-from-out "wires-scratch-pad.rkt"))

(provide wires-operator)

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




(provide (rename-out [wires-expander #%module-begin]))
(define-syntax-rule (wires-expander EXPR ...)
  (#%module-begin
     (displayln "wires expander ")
     EXPR ...))














;; boilerplate below from "raco new wires"

#|

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))

|#
