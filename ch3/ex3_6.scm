(use compat.sicp)

(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define random-init 1)

(define (rand-update x)
    (remainder (* 48271 x) (- (pow 2 31) 1)))

(define rand
    (let ((x random-init))
        (lambda (symbol)
            (cond ((eq? symbol 'generate)
                    (set! x (rand-update x))
                    x)
                  ((eq? symbol 'reset)
                    (lambda (new-value)
                        (set! x new-value)
                        new-value))
                  (else
                    (error "Invalid symbol" symbol))))))

(print (rand 'generate))
(print (rand 'generate))
(print (rand 'generate))
(print ((rand 'reset) 1))
(print (rand 'generate))
