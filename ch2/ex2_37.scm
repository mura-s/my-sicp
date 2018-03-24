(use compat.sicp)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op
                          init
                          (map (lambda (ss) (car ss)) seqs))
              (accumulate-n op
                            init
                            (map (lambda (ss) (cdr ss)) seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define l1 (list 1 2 3))
(define l2 (list 2 3 4))
(define l3 (list 0 1 1))
(print (dot-product l1 l2))

(define (matrix-*-vector m v)
    (map (lambda (mi) (dot-product mi v)) m))

(define m1 (list l1 l2 l3))
(print (matrix-*-vector m1 l1))

(define (transpose mat)
    (accumulate-n cons nil mat))

(print m1)
(print (transpose m1))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (mi)
            (map (lambda (nj)
                    (dot-product mi nj))
                  cols))
             m)))

(define m2 (transpose m1))
(print (matrix-*-matrix m1 m2))
