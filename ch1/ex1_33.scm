(use compat.sicp)

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

(define (inc n) (+ n 1))

;; filtered-accumulate
(define (filtered-accumulate filter combiner null-value term a next b)
    (cond ((> a b) null-value)
          ((filter a)
            (combiner (term a)
                      (filtered-accumulate filter combiner null-value term (next a) next b)))
          (else (filtered-accumulate filter combiner null-value term (next a) next b))))

;; a.
(print (filtered-accumulate prime? + 0 square 2 inc 7))

;; b.
(define (identity x) x)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (co-prime-product n)
    (define (co-prime? a)
        (= 1 (gcd n a)))    
    (filtered-accumulate co-prime? * 1 identity 1 inc (- n 1)))

(print (co-prime-product 5))
(print (co-prime-product 10))