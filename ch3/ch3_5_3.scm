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

; 3.5.3
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
                     (stream-map (lambda (guess)
                                    (sqrt-improve guess x))
                                 guesses)))
    guesses)

; (display-stream (sqrt-stream 2))

(define (pi-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
    (scale-stream (partial-sums (pi-summands 1)) 4))

; (stream-head pi-stream 10)

(define (euler-transform s)
    (let ((s0 (stream-ref s 0))           ; Sn-1
          (s1 (stream-ref s 1))           ; Sn
          (s2 (stream-ref s 2)))          ; Sn+1
        (cons-stream (- s2 (/ (square (- s2 s1))
                              (+ s0 (* -2 s1) s2)))
                    (euler-transform (stream-cdr s)))))

; (stream-head (euler-transform pi-stream) 10)

(define (make-tableau transform s)
    (cons-stream s
                 (make-tableau transform
                               (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-car
                (make-tableau transform s)))

; (stream-head (accelerated-sequence euler-transform pi-stream) 10)
