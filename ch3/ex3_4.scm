(use compat.sicp)

(define (make-account balance password)
    (let ((incorrect-count 0))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                    balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (incorrect-password amount)
            "Incorrect password")
        (define (call-the-cops) (error "CALL-THE-COPS"))
        (define (dispatch p m)
            (if (eq? p password)
                (begin
                    (set! incorrect-count 0)
                    (cond ((eq? m 'withdraw) withdraw)
                          ((eq? m 'deposit) deposit)
                          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
                (begin
                    (set! incorrect-count (+ incorrect-count 1))
                    (if (>= incorrect-count 7)
                        (call-the-cops)
                        incorrect-password))))
        dispatch))

(define acc (make-account 100 'secret-password))

(print ((acc 'other-password 'deposit) 50))
(print ((acc 'secret-password 'withdraw) 40))

(define (incorrect-loop n)
    (if (> n 0)
        (begin
            ((acc 'hoge-password 'withdraw) 10)
            (incorrect-loop (- n 1)))))

(incorrect-loop 7)
