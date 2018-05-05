(use compat.sicp)

(define (count-pairs x)
    (let ((visited-pairs nil))
        (define (visit x)
            (if (not (pair? x))
                0
                (if (memq x visited-pairs)
                    0
                    (begin (set! visited-pairs (cons x visited-pairs))
                           (+ (visit (car x))
                              (visit (cdr x))
                              1)))))
        (visit x)))

; test case 1
(define x1 (cons 'a 'b))
(define x2 (cons 'c 'd))
(define return3 (cons x1 x2))
(print (count-pairs return3))

; test case 2
(define y1 (cons 'a 'b))
(define y2 (cons 'c y1))
(define return4 (cons y1 y2))
(print (count-pairs return4))

; test case 3
(define z1 (cons 'a 'b))
(define z2 (cons z1 z1))
(define return7 (cons z2 z2))
(print (count-pairs return7))
