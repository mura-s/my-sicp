(use compat.sicp)

; 3.5.1
(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (sum-primes a b)
    (define (iter count accum)
        (cond ((> count b) accum)
              ((prime? count) (iter (+ count 1) (+ count accum)))
              (else (iter (+ count 1) accum))))
    (iter a 0))

; (print (sum-primes 2 10))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (sum-primes-2 a b)
    (accumulate +
                0
                (filter prime? (enumerate-interval a b))))

; (print (sum-primes-2 2 10))

; (print (car (cdr (filter prime?
;                   (enumerate-interval 10000 1000000)))))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
    (stream-for-each display-line s))

(define (display-line x)
    (newline)
    (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

; The stream implementation in action
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream)
            the-empty-stream)
          ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                         (stream-filter pred (stream-cdr stream))))
          (else
             (stream-filter pred (stream-cdr stream)))))

; (print
;     (stream-car
;         (stream-cdr
;             (stream-filter prime?
;                         (stream-enumerate-interval 10000 1000000)))))

; Implementing delay and force
(define (force delayed-object)
    (delayed-object))

(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda ()
            (if (not already-run?)
                (begin (set! result (proc))
                       (set! already-run? true)
                       result)
                result))))
