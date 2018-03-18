(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* 100 (/ (width i) (center i))))

(define i1 (make-interval 6.12 7.48))

(print (percent i1))

(define (make-center-percent c p)
    (let ((w (abs (* c (/ p 100)))))
        (make-interval (- c w)
                       (+ c w))))

(print (make-center-percent (center i1) (percent i1)))
