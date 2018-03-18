(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) ( upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))

(define i1 (make-interval 2 3))
(define i2 (make-interval 1 2))
(define i3 (make-interval -1 2))

(print (div-interval i1 i2))
(print (div-interval i1 i3))

;; 0をまたいだ区間で除算した場合、0に近い値で除算した結果の+∞ or -∞が、
;; 最大値 or 最小値に現れるはずだが、そうなっていない。

(define (fixed-div-interval x y)
    (if (> (* (lower-bound y) (upper-bound y)) 0)
        (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))
        (error "fixed-div-interval error")))

(print (fixed-div-interval i1 i2))
(print (fixed-div-interval i1 i3))