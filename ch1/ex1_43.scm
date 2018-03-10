(define (inc a) (+ a 1))

(define (identity a) a)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (rec g i)
        (if (= i n)
             g
             (rec (compose f g) (+ i 1))))
    (rec identity 0))

(print ((repeated inc 10) 2))
(print ((repeated square 2) 5))
