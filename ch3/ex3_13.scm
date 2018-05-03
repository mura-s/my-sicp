(use compat.sicp)

(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define z (make-cycle (list 'a 'b 'c)))

; xの末尾のポインタがxの先頭を指すような循環した図になる.
;
; (last-pair z) を計算しようとすると、無限ループになる.
