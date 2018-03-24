(use compat.sicp)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y))
                nil
                sequence))

(print (map square (list 1 2 3 4 5)))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(print (append (list 1 2 3) (list 4 5 6)))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y))
                0
                sequence))

(print (length (list 1 2 3 4 5)))
