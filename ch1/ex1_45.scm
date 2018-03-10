(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (average a b)
    (/ (+ a b) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (identity a) a)

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (rec g i)
        (if (= i n)
             g
             (rec (compose f g) (+ i 1))))
    (rec identity 0))

(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define (n-root n x)
    (define (num-repeat k i)
        (if (< k 2) i (num-repeat (/ k 2) (+ i 1))))
    (fixed-point
        ((repeated average-damp (num-repeat n 0)) (lambda (y) (/ x (pow y (- n 1)))))
        1.0))

(print (n-root 4 81))
(print (n-root 8 6561))

;; 2^k <= n < 2^(k+1) のとき、k回の平均緩和が必要
;; これを利用し、n-rootを実装した