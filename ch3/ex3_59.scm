(use compat.sicp)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))

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

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))

(define (stream-head s n)
    (define (iter s n)
        (if (<= n 0)
            'done
            (begin
                (display (stream-car s))
                (newline)
                (iter (stream-cdr s) (- n 1)))))
    (iter s n))

; a.
(define (integrate-series s)
    (define is (mul-streams s (stream-map / ones integers)))
    is)

(stream-head (integrate-series ones) 10)
(newline)

; b.
(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(stream-head exp-series 10)
(newline)

(define cosine-series
    (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
  
(define sine-series
    (cons-stream 0 (integrate-series cosine-series)))

(stream-head cosine-series 10)
(newline)
(stream-head sine-series 10)
(newline)
