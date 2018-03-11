(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (cond ((and (>= n 0) (>= d 0)) 
            (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))))
          ((and (>= n 0) (< d 0)) 
            (let ((g (gcd n (- d))))
                (cons (- (/ n g)) (/ (- d) g))))
          ((and (< n 0) (>= d 0))
            (let ((g (gcd (- n) d)))
                (cons (- (/ (- n) g)) (/ d g))))
          (else
            (let ((g (gcd (- n) (- d))))
                (cons (/ (- n) g) (/ (- d) g))))))

(print-rat (make-rat 1 3))
(print-rat (make-rat 1 -3))
(print-rat (make-rat -1 3))
(print-rat (make-rat -1 -3))

;; よりシンプルな解答
;; ref. http://community.schemewiki.org/?sicp-ex-2.1
;; (define (make-rat n d)
;;   (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
;;     (cons (/ n g) (/ d g))))
