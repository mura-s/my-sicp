(use compat.sicp)

(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high))
          (hsq (* high high)))
        (let ((j (an-integer-between i high)))
            (let ((ksq (+ (* i i) (* j j))))
                (require (>= hsq ksq))
                (let ((k (sqrt ksq)))
                    (require (integer? k))
                    (list i j k))))))

; Benの方法は、kをjからhighまで探索する必要がなくなるので、効率が良い。
