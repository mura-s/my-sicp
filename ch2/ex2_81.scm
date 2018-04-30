(use compat.sicp)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; a.
; 無限にapply-genericが呼び出され続け、無限ループになってしまう.

; b.
; Louisは正しくない. このままで正しく動く.

; c.
(define (apply-generic op . args)
    (define (no-method-error type-tags)
        (error "No method for these types" (list op type-tags)))
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                            (cond ((eq? type1 type2) (no-method-error type-tags))
                                  (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                  (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                                  (else (no-method-error type-tags)))))
                    (no-method-error type-tags))))))
