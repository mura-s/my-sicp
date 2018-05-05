(use compat.sicp)

(define (cycle? x)
    (let ((visited nil))
        (define (iter x)
            (cond ((not (pair? x)) false)
                  ((memq x visited) true)
                  (else
                    (set! visited (cons x visited))
                    (iter (cdr x)))))
        (iter x)))

; 循環するリスト
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)
(define z (make-cycle (list 'a 'b 'c)))
(print (cycle? z))

; 循環しないリスト
(define x (list 1 2 3))
(print (cycle? x))
