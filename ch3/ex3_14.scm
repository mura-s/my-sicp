(use compat.sicp)

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))

(define v (list 'a 'b 'c 'd))

; mysteryはreverseしたlistを返す.
; その際、引数のlistはcarのみのlistになる.

(define w (mystery v))
(print v)
(print w)
