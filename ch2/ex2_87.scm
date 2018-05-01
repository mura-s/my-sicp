(use compat.sicp)

(define (install-polynomial-package)
    ; ...
    (put '=zero? '(polynomial)
        (lambda (x) (empty-termlist? (term-list x))))
    ; ...
    'done)