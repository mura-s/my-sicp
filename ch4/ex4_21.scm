(use compat.sicp)

; a.
; 階乗を計算することを調べる
(print 
    ((lambda (n)
        ((lambda (fact)
            (fact fact n))
            (lambda (ft k)
                (if (= k 1)
                    1
                    (* k (ft ft (- k 1)))))))
    10))

; Fibonacci数の計算
(print 
    ((lambda (n)
        ((lambda (fact)
            (fact fact n))
            (lambda (ft k)
                (cond ((= k 0) 0)
                      ((= k 1) 1)
                      (else (+ (ft ft (- k 1))
                               (ft ft (- k 2))))))))
    10))

; b.
(define (f x)
    ((lambda (even? odd?)
        (even? even? odd? x))
        (lambda (ev? od? n)
            (if (= n 0) true (od? ev? od? (- n 1))))
        (lambda (ev? od? n)
            (if (= n 0) false (ev? ev? od? (- n 1))))))

(print (f 10))
(print (f 11))
