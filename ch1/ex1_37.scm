;; a. 再帰的プロセス
(define (cont-frac n d k)
    (define (rec i)
        (if (> i k) 0
                    (/ (n i)
                       (+ (d i) (rec (+ i 1))))))
    (rec 1))

(define (k-cont-frac k)
    (cont-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               k))

(print (k-cont-frac 10))

;; b. 反復的プロセス
(define (cont-frac-iter n d k)
    (define (iter i result)
        (if (= i 0) result
                    (iter (- i 1)
                          (/ (n i) (+ (d i) result)))))
    (iter k 0))

(define (k-cont-frac-iter k)
    (cont-frac-iter (lambda (i) 1.0)
                    (lambda (i) 1.0)
                    k))

(print (k-cont-frac-iter 10))