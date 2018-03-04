(define (cont-frac n d k)
    (define (rec i)
        (if (> i k) 0
                    (/ (n i)
                       (+ (d i) (rec (+ i 1))))))
    (rec 1))

(define (e-cont-frac k)
    (+ 2.0
       (cont-frac (lambda (i) 1.0)
                  (lambda (i) (if (= (mod i 3) 2)
                                      (+ 2.0 (* (div i 3) 2.0))
                                      1.0))
               k)))

(print (e-cont-frac 10))
(print (e-cont-frac 100))