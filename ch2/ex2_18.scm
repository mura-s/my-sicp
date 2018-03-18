(use compat.sicp)

(define (reverse items)
    (define (iter a rev)
        (if (null? a)
            rev
            (iter (cdr a) (cons (car a) rev))))
    (iter items nil))

(define squares (list 1 4 9 16 25))

(print (reverse squares))