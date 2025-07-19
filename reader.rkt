#lang s-exp syntax/module-reader
racket/base
#:read wires-read
#:read-syntax wires-read-syntax


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
    (datum->syntax #f module-datum))
