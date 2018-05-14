(use compat.sicp)

; 3.4.1

;(define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;                balance)
;        "Insufficient funds"))

; 3.4.2
; Serializers in Scheme
(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((protected (make-serializer)))
        (define (dispatch m)
        (cond ((eq? m 'withdraw) (protected withdraw))
              ((eq? m 'deposit) (protected deposit))
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
        dispatch))
