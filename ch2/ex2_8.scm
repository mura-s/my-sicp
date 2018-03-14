(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y))
                                   (- (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define i1 (make-interval 2 3))
(define i2 (make-interval -3 -1))

(print (sub-interval i1 i2))