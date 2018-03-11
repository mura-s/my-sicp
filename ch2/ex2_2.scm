(define (make-segment p1 p2)
    (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-point x y)
    (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
    (/ (+ x y) 2))

(define (midpoint-segment s)
    (let ((ss (start-segment s))
          (es (end-segment s)))
         (make-point (average (x-point ss) (x-point es))
                     (average (y-point ss) (y-point es)))))

(print-point (midpoint-segment
                (make-segment
                    (make-point 1 2)
                    (make-point 3 4))))