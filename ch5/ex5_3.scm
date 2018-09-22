(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))

; good-enough?とimprove演算が基本演算として使える場合のsqrt
(controller
    (assign guess (const 1.0))
    test-ge
        (test (op good-enough?) (reg guess))
        (branch (label sqrt-done))
        (assign guess (op improve) (reg guess))
        (goto (label test-ge))
    sqrt-done)

; good-enough?とimprove演算が基本演算として使えない場合のsqrt
(controller
    (assign guess (const 1.0))
    test-ge
        (assign t1 (op square) (reg guess))
        (assign t2 (op -) (reg t1) (reg x))
        (assign t3 (op abs) (reg t2))
        (test (op <) (reg t3) (const 0.001))
        (branch (label sqrt-done))
        (assign s1 (op /) (reg x) (reg guess))
        (assign guess (op average) (reg guess) (reg s1))
        (goto (label test-ge))
    sqrt-done)
