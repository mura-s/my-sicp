(use compat.sicp)

(define (square-list items)
    (if (null? items)
        nil
        (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
    (map square items))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

(print (square-list (list 1 2 3 4)))
(print (square-list-map (list 1 2 3 4)))
