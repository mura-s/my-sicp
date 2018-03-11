(define (iterative-improve good-enough? improve)
    (define (iter guess)
        (if (good-enough? guess)
            guess
            (iter (improve guess))))
    (lambda (guess) (iter guess)))

(define tolerance 0.00001)

(define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

(define (average x y)
    (/ (+ x y) 2))

(define (sqrt x)
    ((iterative-improve
        (lambda (y) (good-enough? (square y) x))
        (lambda (y) (average y (/ x y))))
     1.0))

(print (sqrt 9))

(define (fixed-point f first-guess)
    ((iterative-improve
        (lambda (y) (good-enough? y (f y)))
        (lambda (y) (f y)))
     first-guess))

(define (sqrt2 x)
    (fixed-point (lambda (y) (average y (/ x y)))
                 1.0))

(print (sqrt2 9))