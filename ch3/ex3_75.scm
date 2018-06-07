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

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))

(define sense-data (integers-starting-from -3))

(define (sign-change-detector value last-value)
    (if (>= last-value 0)
        (if (< value 0) -1 0)
        (if (>= value 0) 1 0)))

(define (make-zero-crossings input-stream last-avpt last-value)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream (sign-change-detector avpt last-avpt)
                     (make-zero-crossings (stream-cdr input-stream)
                                          avpt
                                          (stream-car input-stream)))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

(stream-head zero-crossings 10)
