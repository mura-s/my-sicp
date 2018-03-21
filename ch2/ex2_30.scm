(use compat.sicp)

(define (square-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

(print (square-tree (list 1
                          (list 2 (list 3 4) 5)
                          (list 6 7))))

(define (square-tree2 tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree2 sub-tree)
                (square sub-tree)))
         tree))

(print (square-tree2 (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7))))
