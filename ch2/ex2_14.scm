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

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* 100 (/ (width i) (center i))))

(define (make-center-percent c p)
    (let ((w (abs (* c (/ p 100)))))
        (make-interval (- c w)
                       (+ c w))))

(define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1))) 
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(define ia (make-interval 2 8))
(define ib (make-interval 3 5))

(print (par1 ia ib))
(print (par2 ia ib))
;; Lemの言うとおり異なる結果になる

(define a (make-interval 2 8))
(define b (make-interval 3 5))
(define c (make-interval 10000 10001))

(print (percent a))
(print (percent (div-interval a a)))
(print (percent (div-interval a b)))
(print (percent (div-interval a c)))
;; 誤差が大きいもの同士を演算するとさらに誤差が大きくなっている.
;; 一方、片方の誤差が小さい場合は、誤差はほとんど変わらない.
