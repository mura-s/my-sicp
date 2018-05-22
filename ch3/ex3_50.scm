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

(print (stream-map
            +
            (cons-stream 1 (cons-stream 2 the-empty-stream))
            (cons-stream 4 (cons-stream 5 the-empty-stream))))
