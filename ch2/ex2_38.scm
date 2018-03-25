(use compat.sicp)

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))

(print (fold-right / 1 (list 1 2 3)))

(print (fold-left / 1 (list 1 2 3)))

(print (fold-right list nil (list 1 2 3)))

(print (fold-left list nil (list 1 2 3)))

; どのような並びに対しても同じ値を生じるためには、
; opが可換律と結合律を満たしている必要がある。
