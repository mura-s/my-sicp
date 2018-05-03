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
