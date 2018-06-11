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

(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define random-init 1)

(define (rand-update x)
    (remainder (* 48271 x) (- (pow 2 31) 1)))

(define (random-numbers input-stream random-init)
    (define random-stream
        (if (stream-null? input-stream)
            the-empty-stream
            (let ((request (stream-car input-stream)))
                (cons-stream
                    (cond ((eq? request 'generate) (rand-update random-init))
                          ((and (pair? request) (eq? (car request) 'reset))
                            (rand-update (cdr request)))
                          (else (error "Invalid request" request)))
                    (random-numbers (stream-cdr input-stream) (stream-car random-stream))))))
    random-stream)

(define request-stream
    (cons-stream (cons 'reset 100)
                 (cons-stream 'generate
                              (cons-stream 'generate
                                           (cons-stream (cons 'reset 100)
                                                        (cons-stream 'generate
                                                                     (cons-stream 'generate
                                                                                  the-empty-stream)))))))

(stream-head (random-numbers request-stream 1) 6)
