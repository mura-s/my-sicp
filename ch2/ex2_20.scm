(use compat.sicp)

(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))

(define (evens? a b) (and (even? a) (even? b)))
(define (odds? a b) (and (odd? a) (odd? b)))

(define (same-parity n . ms)
    (define (rec items)
        (cond ((null? items) nil)
              ((or (evens? n (car items)) (odds? n (car items)))
                (cons (car items) (rec (cdr items))))
              (else (rec (cdr items)))))
    (cons n (rec ms)))

(print (same-parity 1 2 3 4 5 6 7))
(print (same-parity 2 3 4 5 6 7))
