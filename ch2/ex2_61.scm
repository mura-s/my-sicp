(use compat.sicp)

(define (adjoin-set x set)
    (cond ((null? set)
            (list x))
          ((= x (car set))
            set)
          ((< x (car set))
            (cons x set))
          (else
            (cons (car set)
                  (adjoin-set x (cdr set))))))

(print (adjoin-set 1 (list 2 4 6)))
(print (adjoin-set 2 (list 2 4 6)))
(print (adjoin-set 3 (list 2 4 6)))
(print (adjoin-set 7 (list 2 4 6)))