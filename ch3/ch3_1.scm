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

; 3.1.2
(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define random-init 1)

(define (rand-update x)
    (remainder (* 48271 x) (- (pow 2 31) 1)))

(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x)))

(define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
    (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
    (define (iter trials-remaininig trials-passed)
        (cond ((= trials-remaininig 0)
                (/ trials-passed trials))
              ((experiment)
                (iter (- trials-remaininig 1) (+ trials-passed 1)))
              (else
                (iter (- trials-remaininig 1) trials-passed))))
    (iter trials 0))

; (print (estimate-pi 10000))

(define (estimate-pi-2 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
    (define (iter trials-remaining trials-passed x)
        (let ((x1 (rand-update x)))
            (let ((x2 (rand-update x1)))
                (cond ((= trials-remaining 0)   
                        (/ trials-passed trials))
                      ((= (gcd x1 x2) 1)
                        (iter (- trials-remaining 1)
                              (+ trials-passed 1)
                              x2))
                    (else
                        (iter (- trials-remaining 1)
                              trials-passed
                              x2))))))
    (iter trials 0 initial-x))

; (print (estimate-pi-2 10000))
