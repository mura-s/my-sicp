(use compat.sicp)

(define (install-polynomial-package)
    ; ...
    (define (negate-poly p)
        (make-poly (variable p)
                   (negate-terms (term-list p))))

    (define (negate-terms L)
        (if (empty-termlist? L)
            (the-empty-termlist)
            (let ((t (first-term L)))
                (adjoin-term (make-term (order t) (negate (coeff t)))
                             (negate-terms (rest-term L))))))

    (put 'negate 'polynomial
        (lambda (p) (tag (negate-poly p))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 (negate p2)))))
    ; ...
    'done)

(define (negate x) (apply-generic 'negate x))

; scheme-numberやrationalなどにもnegate演算の定義が必要.
