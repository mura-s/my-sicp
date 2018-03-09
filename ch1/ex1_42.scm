(define (inc a) (+ a 1))

(define (compose f g)
    (lambda (x) (f (g x))))

(print ((compose square inc) 6))
