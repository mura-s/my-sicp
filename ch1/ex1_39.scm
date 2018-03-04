(define (cont-frac n d k)
    (define (rec i)
        (if (> i k) 0
                    (/ (n i)
                       (+ (d i) (rec (+ i 1))))))
    (rec 1))

(define (tan-cf x k)
    (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
               (lambda (i) (- (* 2 i) 1))
               k))

(print (tan-cf 1.0 10))