(use compat.sicp)

(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

(print (list 'a 'b 'c))
(print (list (list 'george)))
(print (cdr '((x1 x2) (y1 y2))))
(print (cadr '((x1 x2) (y1 y2))))
(print (pair? (car '(a short list))))
(print (memq 'red '((red shoes) (blue socks))))
(print (memq 'red '(red shoes blue socks)))
