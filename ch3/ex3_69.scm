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

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                   (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (partial-sums s)
    (cons-stream (stream-car s)
                 (add-streams (partial-sums s) (stream-cdr s))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
    (newline)
    (display x))

(define (display-stream s)
    (stream-for-each display-line s))

(define (stream-head s n)
    (define (iter s n)
        (if (<= n 0)
            'done
            (begin
                (display (stream-car s))
                (newline)
                (iter (stream-cdr s) (- n 1)))))
    (iter s n))

(define (average x y)
    (/ (+ x y) 2))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

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

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
    (cons-stream
        (list (stream-car s) (stream-car t) (stream-car u))
        (interleave
            (stream-map (lambda (x) (cons (stream-car s) x))
                        (stream-cdr (pairs t u)))
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(stream-head (triples integers integers integers) 10)
(newline)

(define (pythagorarean? a b c)
    (= (square c)
       (+ (square a) (square b))))

(define (pythagoras ts)
    (stream-filter (lambda (t) (pythagorarean? (car t) (cadr t) (caddr t)))
                   ts))

(stream-head (pythagoras (triples integers integers integers)) 5)
