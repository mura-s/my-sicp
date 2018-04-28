(use compat.sicp)

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
    ; ...
    (put 'equ? '(scheme-number scheme-number) =)
    ; ...
    'done)

(define (install-rational-package)
    ; ...
    (put 'equ? '(rational rational)
        (lambda (x y) (= (* (numer x) (denom y))
                         (* (numer y) (denom x)))))
    ; ...
    'done)

(define (install-complex-package)
    ; ...
    (put 'equ? '(complex complex)
        (lambda (x y) (and (= (real-part x) (real-part y))
                           (= (imag-part x) (imag-part y)))))
    ; ...
   'done)
