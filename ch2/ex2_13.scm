(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* 100 (/ (width i) (center i))))

(define (mul-interval x y)
    (make-interval (* (lower-bound x) (lower-bound y))
                   (* (upper-bound x) (upper-bound y))))

(define i1 (make-interval 3.0 3.1))
(define i2 (make-interval 2.0 2.1))

(define (approximate-error-percent i1 i2)
    (+ (percent i1) (percent i2)))

(print (approximate-error-percent i1 i2))
(print (percent (mul-interval i1 i2)))
