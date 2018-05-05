(use compat.sicp)

(define (floyd-find-cycle x)
    (define (safe-cdr x)
        (if (pair? x)
            (cdr x)
            nil))

    (define (iter slow fast)
        (cond ((null? slow) false)
              ((eq? slow fast) true)
              (else
                (iter (safe-cdr slow) (safe-cdr (safe-cdr fast))))))

    (iter (safe-cdr x) (safe-cdr (safe-cdr x))))

; 循環するリスト
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)
(define z (make-cycle (list 'a 'b 'c)))
(print (floyd-find-cycle z))

; 循環しないリスト
(define x (list 1 2 3))
(print (floyd-find-cycle x))
