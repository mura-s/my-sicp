(use srfi-27) ; for random-real

(define (pow y n)
    (if (= n 0)
        1
        (* y (pow y (- n 1)))))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (* range (random-real)))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaininig trials-passed)
        (cond ((= trials-remaininig 0)
                (/ trials-passed trials))
              ((experiment)
                (iter (- trials-remaininig 1) (+ trials-passed 1)))
              (else
                (iter (- trials-remaininig 1) trials-passed))))
    (iter trials 0))

(define (integration-test x1 x2 y1 y2)
    (define (distance center-x center-y x y)
        (sqrt (+ (pow (- center-x x) 2)
                 (pow (- center-y y) 2))))
    (lambda ()
        (let ((x (random-in-range x1 x2))
              (y (random-in-range y1 y2))
              (r (/ (- x2 x1) 2)))
            (let ((center-x (- x2 r))
                  (center-y (- y2 r)))
                (<= (distance center-x center-y x y) r)))))

(define (estimate-integral p x1 x2 y1 y2 trials)
    (define calc-area (* (- x2 x1) (- y2 y1)))
    (* calc-area (monte-carlo trials (p x1 x2 y1 y2))))

(define (estimate-pi trials)
    (estimate-integral integration-test 0.0 2.0 0.0 2.0 trials))

(print (estimate-pi 10000))
