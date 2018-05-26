(use compat.sicp)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream)
            the-empty-stream)
          ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                         (stream-filter pred (stream-cdr stream))))
          (else
             (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

; (print (stream-car integers))
; (print (stream-cdr integers))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
    (stream-filter
        (lambda (x) (not (divisible? x 7)))
        integers))

; (print (stream-ref no-sevens 100))

(define (fibgen a b)
    (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

; (print (stream-ref fibs 5))

(define (sieve stream)
    (cons-stream
        (stream-car stream)
        (sieve (stream-filter
                    (lambda (x) (not (divisible? x (stream-car stream))))
                    (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

; (print (stream-ref primes 50))

; Defining streams implicitly
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                   (cons proc (map stream-cdr argstreams))))))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

; (print (stream-ref integers 10))

(define fibs
    (cons-stream 0
                (cons-stream 1
                             (add-streams (stream-cdr fibs)
                                          fibs))))

; (print (stream-ref fibs 5))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))

(define double (cons-stream 1 (scale-stream double 2)))

; (print (stream-ref double 3))

(define (prime? n)
    (define (iter ps)
        (cond ((> (square (stream-car ps)) n) true)
              ((divisible? n (stream-car ps)) false)
              (else (iter (stream-cdr ps)))))
    (iter primes))

(define primes
    (cons-stream
        2
        (stream-filter prime? (integers-starting-from 3))))
  
; (print (stream-ref primes 5))
