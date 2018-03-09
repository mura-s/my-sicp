(define (inc a) (+ a 1))

(define (double f)
    (lambda (x) (f (f x))))

(print ((double inc) 5))
(print (((double (double double)) inc) 5))