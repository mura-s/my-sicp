;; 黄金比は、 φ^2 = φ + 1 を満たすので、式変形により、 x -> 1 + 1/x がφの不動点となる。

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

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                                  1.0))

(print golden-ratio)