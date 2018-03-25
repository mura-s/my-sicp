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

(define (reverse-r sequence)
    (fold-right (lambda (x acc) (append acc (list x))) nil sequence))

(define (reverse-l sequence)
    (fold-left (lambda (acc x) (cons x acc)) nil sequence))

(define squares (list 1 4 9 16 25))
(print (reverse-r squares))
(print (reverse-l squares))
