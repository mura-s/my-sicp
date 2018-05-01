(use compat.sicp)

; 3.1.1
(define new-withdraw
    (let ((balance 100))
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))))

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch)

(define acc (make-account 100))

; (print ((acc 'withdraw) 50))
; (print ((acc 'deposit) 10))
; (print ((acc 'withdraw) 70))
; (print ((acc 'withdraw) 30))
