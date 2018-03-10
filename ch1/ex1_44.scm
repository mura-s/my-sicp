(define (identity a) a)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (rec g i)
        (if (= i n)
             g
             (rec (compose f g) (+ i 1))))
    (rec identity 0))

(define dx 0.00001)

(define (smooth f)
    (lambda (x)
        (/ (+ (f (- x dx))
              (f x)
              (f (+ x dx)))
           3)))

(print ((smooth square) 5))

(define (repeated-smooth f n)
    ((repeated smooth n) f))

(print ((repeated-smooth square 3) 5))