#lang racket/base

(provide
  (rename-out
   [my-read        read]
   [my-read-syntax read-syntax]))

(require
  racket/port
  racket/string)

(define (my-read in)
  (syntax->datum
   (my-read-syntax #false in)))

(define (operator line)
  (open-input-string (format "(wires-operator ~a)" line)))

(define (my-read-syntax src port)
  (define src-datums
    (for/list ([line (in-list (port->lines port))]
               #:when (non-empty-string? line))
      (read (operator line))))
  (define module-datum
    `(module wires racket/base
       (require wires/lang/wires-syntax)
       ,@src-datums))
  (datum->syntax
   #false module-datum))
