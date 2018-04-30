(use compat.sicp)

(define (raise x) (apply-generic 'raise x))

(define (install-scheme-number-package)
    ; ...
    (define (raise-to-rational x) (make-rational x 1))
    (put 'raise '(scheme-number) raise-to-rational)
    ; ...
    'done)

(define (install-rational-package)
    ; ...
    (define (raise-to-real-number x)
        (make-real-number (/ (* 1.0 (numer x)) (denom x))))
    (put 'raise '(rational) raise-to-real-number)
    ; ...
    'done)

(define (install-real-number-package)
    ; ...
    (define (raise-to-complex x)
        (make-complex-from-real-imag x 0))
    (put 'raise '(real-number) raise-to-complex)
    ; ...
   'done)

