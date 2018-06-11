(use compat.sicp)
(use srfi-27) ; for random-real

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

(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* range (random-real)))))

(define (integration-test x1 x2 y1 y2)
    (define (distance center-x center-y x y)
        (sqrt (+ (pow (- center-x x) 2)
                 (pow (- center-y y) 2))))
    (lambda ()
        (let ((x (random-in-range x1 x2))
              (y (random-in-range y1 y2))
              (r (/ (- x2 x1) 2)))
            (let ((center-x (- x2 r))
                  (center-y (- y2 r)))
                (<= (distance center-x center-y x y) r)))))

(define (test-stream p x1 x2 y1 y2)
    (cons-stream
         ((p x1 x2 y1 y2))
         (test-stream p x1 x2 y1 y2)))

(define (monte-carlo experiment-stream passed failed)
    (define (next passed failed)
        (cons-stream
            (/ passed (+ passed failed))
            (monte-carlo
                (stream-cdr experiment-stream) passed failed)))
    (if (stream-car experiment-stream)
        (next (+ passed 1) failed)
        (next passed (+ failed 1))))

(define (estimate-integral p x1 x2 y1 y2)
    (define calc-area (* (- x2 x1) (- y2 y1)))
    (stream-map (lambda (p) (* p calc-area))
                (monte-carlo (test-stream p x1 x2 y1 y2) 0 0)))

(define estimate-pi
    (estimate-integral integration-test 0.0 2.0 0.0 2.0))

(stream-head estimate-pi 10000)
