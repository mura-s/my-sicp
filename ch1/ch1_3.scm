(use compat.sicp)

;; 1.3.1
(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (sum-cubes a b)
    (sum cube a inc b))

;; (print (sum-cubes 3 5))

(define (identity x) x)

(define (sum-integers a b)
    (sum identity a inc b))

;; (print (sum-integers 3 5))

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x) (+ x 4))
    (sum pi-term a pi-next b))

;; (print (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))

;; (print (integral cube 0 1 0.01))
;; (print (integral cube 0 1 0.001))

;; 1.3.2
(define (pi-sum-lambda a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
         a
         (lambda (x) (+ x 4))
         b))

;; (print (* 8 (pi-sum-lambda 1 1000)))

(define (f x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
        (+ (* x (square a))
           (* y b)
           (* a b))))

;; (print (f 2 3))

;; 1.3.3
(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

(define (average a b)
    (/ (+ a b) 2))

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) (search f neg-point midpoint))
                      ((negative? test-value) (search f midpoint pos-point))
                      (else midpoint))))))

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
              ((and (negative? b-value) (positive? a-value)) (search f b a))
              (else (error "Values are not of opposite sign" a b)))))

;; (print (half-interval-method sin 2.0 4.0))
;; (print (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;;                              1.0
;;                              2.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

;; (print (fixed-point cos 1.0))

(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y)))
                 1.0))

;; (print (sqrt 2))

;; 1.3.4
(define (average-damp f)
    (lambda (x) (average x (f x))))

;; (print ((average-damp square) 10))

(define (sqrt-ad x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))

;; (print (sqrt-ad 2))

(define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (square y))))
                 1.0))

;; (print (cube-root 2))

(define dx 0.00001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x))
                   dx)))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
    (newtons-method (lambda (y) (- (square y) x))
                    1.0))

;; (print (sqrt-newton 2))

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))

(define (sqrt-1 x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                              average-damp
                              1.0))

(define (sqrt-2 x)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
                              newton-transform
                              1.0))

;; (print (sqrt-1 2))
;; (print (sqrt-2 2))
