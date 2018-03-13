(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define (exp z b n)
    (if (= (remainder z b) 0)
        (exp (/ z b) b (+ n 1))
        n))

(define (cons a b)
    (* (pow 2 a) (pow 3 b)))

(define (car z)
    (exp z 2 0))

(define (cdr z)
    (exp z 3 0))

(print (car (cons 2 5)))
(print (cdr (cons 2 5)))
