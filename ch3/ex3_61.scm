(use compat.sicp)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                   (cons proc (map stream-cdr argstreams))))))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                              (mul-series (stream-cdr s1) s2))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-head s n)
    (define (iter s n)
        (if (<= n 0)
            'done
            (begin
                (display (stream-car s))
                (newline)
                (iter (stream-cdr s) (- n 1)))))
    (iter s n))

(define (invert-unit-series s)
    (cons-stream 1
                 (mul-series (scale-stream (stream-cdr s) -1)
                             (invert-unit-series s))))

(stream-head (invert-unit-series integers) 10)
