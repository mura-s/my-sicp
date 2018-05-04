(use compat.sicp)

; 3.3.1
(define x (list (cons 'a 'b) 'c 'd))
(define y (list 'e 'f))

(print x)
(print y)

(set-car! x y)
(print x)

(set-cdr! x y)
(print x)

(define (get-new-pair) (cons nil nil))

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

(print (cons 'a 'b))
(newline)

; Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
    (set-car! (car x) 'wow)
    x)

(print (set-to-wow! z1))
(print (set-to-wow! z2))
(newline)
