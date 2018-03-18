(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define (mul-interval x y)
    (define (positive? a) (>= a 0))
    (define (negative? a) (< a 0))
    (let ((x-lo (lower-bound x))
          (x-up (upper-bound x))
          (y-lo (lower-bound y))
          (y-up (upper-bound y)))
    (cond ((and (positive? x-lo) (positive? x-up))
            (cond ((and (positive? y-lo) (positive? y-up))
                    (make-interval (* x-lo y-lo) (* x-up y-up)))
                  ((and (negative? y-lo) (positive? y-up))
                    (make-interval (* x-up y-lo) (* x-up y-up)))
                  ((and (negative? y-lo) (negative? y-up))
                    (make-interval (* x-up y-lo) (* x-lo y-up)))))
          ((and (negative? x-lo) (positive? x-up))
            (cond ((and (positive? y-lo) (positive? y-up))
                    (make-interval (* x-lo y-up) (* x-up y-up)))
                  ((and (negative? y-lo) (positive? y-up))
                    (make-interval (min (* x-lo y-up) (* x-up y-lo))
                                   (max (* x-lo y-lo) (* x-up y-up))))
                  ((and (negative? y-lo) (negative? y-up))
                    (make-interval (* x-up y-lo) (* x-lo y-lo)))))
          ((and (negative? x-lo) (negative? x-up))
            (cond ((and (positive? y-lo) (positive? y-up))
                    (make-interval (* x-lo y-up) (* x-up y-lo)))
                  ((and (negative? y-lo) (positive? y-up))
                    (make-interval (* x-lo y-up) (* x-lo y-lo)))
                  ((and (negative? y-lo) (negative? y-up))
                    (make-interval (* x-up y-up) (* x-lo y-lo))))))))

(define i1 (make-interval 1 2))
(define i2 (make-interval -1 2))
(define i3 (make-interval -2 -1))

(print (mul-interval i1 i1))
(print (mul-interval i1 i2))
(print (mul-interval i1 i3))

(print (mul-interval i2 i1))
(print (mul-interval i2 i2))
(print (mul-interval i2 i3))

(print (mul-interval i3 i1))
(print (mul-interval i3 i2))
(print (mul-interval i3 i3))
