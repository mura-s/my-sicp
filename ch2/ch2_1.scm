;; 2.1.1
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-third one-third))

;; 2.1.4
; (define (add-interval x y)
;     (make-interval (+ (lower-bound x) (lower-bound y))
;                    (+ (upper-bound x) (upper-bound y))))

; (define (mul-interval x y)
;     (let ((p1 (* (lower-bound x) (lower-bound y)))
;           (p2 (* (lower-bound x) (upper-bound y)))
;           (p3 (* (upper-bound x) (lower-bound y)))
;           (p4 (* (upper-bound x) ( upper-bound y))))
;         (make-interval (min p1 p2 p3 p4)
;                        (max p1 p2 p3 p4))))

; (define (div-interval x y)
;     (mul-interval x
;                   (make-interval (/ 1.0 (upper-bound y))
;                                  (/ 1.0 (lower-bound y)))))
