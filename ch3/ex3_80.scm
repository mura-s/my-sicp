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

(define (stream-head2 s n)
    (define (iter s n)
        (if (<= n 0)
            'done
            (begin
                (display (stream-car (car s)))
                (newline)
                (display (stream-car (cdr s)))
                (newline)
                (newline)
                (iter (cons (stream-cdr (car s)) (stream-cdr (cdr s)))
                      (- n 1)))))
    (iter s n))

; 3.5.4
(define (integral delayed-integrand initial-value dt)
    (define int
        (cons-stream initial-value
                     (let ((integrand (force delayed-integrand)))
                        (add-streams (scale-stream integrand dt)
                                     int))))
    int)

(define (RLC R L C dt)
    (lambda (vc0 il0)
        (define vc (integral (delay dvc) vc0 dt))
        (define il (integral (delay dil) il0 dt))
        (define dvc (scale-stream il (- (/ 1 C))))
        (define dil (add-streams (scale-stream vc (/ 1 L))
                                 (scale-stream il (- (/ R L)))))
        (cons vc il)))

(define RLC-stream ((RLC 1 1 0.2 0.1) 10 0))

(stream-head2 RLC-stream 10)
