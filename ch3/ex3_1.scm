(use compat.sicp)

(define (make-accumulator sum)
    (lambda (n)
        (set! sum (+ sum n))
        sum))

(define A (make-accumulator 5))

(print (A 10))
(print (A 10))
