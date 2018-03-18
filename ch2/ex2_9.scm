(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y))
                                   (- (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (add-i-width i1 i2)
    (+ (width i1) (width i2)))

(define (sub-i-width i1 i2)
    (+ (width i1) (width i2)))

(define i1 (make-interval 2 3))
(define i2 (make-interval -3 -1))
(define i3 (make-interval 1 5))

(print (= (add-i-width i1 i2) (width (add-interval i1 i2))))
(print (= (add-i-width i1 i3) (width (add-interval i1 i3))))
(print (= (sub-i-width i1 i2) (width (sub-interval i1 i2))))
(print (= (sub-i-width i1 i3) (width (sub-interval i1 i3))))

;; 乗算について
(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) ( upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (wrong-mul-i-width i1 i2)
    (* (width i1) (width i2)))

(print (= (wrong-mul-i-width i1 i3) (width (mul-interval i1 i3))))
;; この例のように、区間ごとのwidthの乗算は、区間の乗算結果のwidthとは等しくない
;; 除算についても乗算と同様
