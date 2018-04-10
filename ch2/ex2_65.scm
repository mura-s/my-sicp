(use compat.sicp)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

; ex2_63
(define (tree-list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))

; ex2_64
(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
            (let ((left-result (partial-tree elts left-size)))
                (let ((left-tree (car left-result))
                      (non-left-elts (cdr left-result))
                      (right-size (- n (+ left-size 1))))
                    (let ((this-entry (car non-left-elts))
                          (right-result (partial-tree (cdr non-left-elts)
                                                      right-size)))
                        (let ((right-tree (car right-result))
                              (remaining-elts (cdr right-result)))
                            (cons (make-tree this-entry left-tree right-tree)
                                  remaining-elts))))))))

(define (intersection-set set1 set2)
    (define (intersection-list list1 list2) ; 2.3.3 Sets as ordered lists
        (if (or (null? list1) (null? list2))
            '()    
            (let ((x1 (car list1)) (x2 (car list2)))
                (cond ((= x1 x2) (cons x1 (intersection-list (cdr list1) (cdr list2))))
                      ((< x1 x2) (intersection-list (cdr list1) list2))
                      ((< x2 x1) (intersection-list list1 (cdr list2)))))))
    (let ((list1 (tree-list-2 set1))
          (list2 (tree-list-2 set2)))
         (let ((intersect-list (intersection-list list1 list2)))
            (list->tree intersect-list))))

(define set1 (list->tree '(1 3 5 7 9 11)))
(define set2 (list->tree '(1 2 5 6 7 8 11)))

(print (intersection-set set1 set2))

(define (union-set set1 set2)
    (define (union-list list1 list2) ; ex2_62
        (cond ((null? list1) list2)
              ((null? list2) list1)
              (else
                (let ((x1 (car list1)) (x2 (car list2)))
                    (cond ((= x1 x2) (cons x1 (union-list (cdr list1) (cdr list2))))
                          ((< x1 x2) (cons x1 (union-list (cdr list1) list2)))
                          ((> x1 x2) (cons x2 (union-list list1 (cdr list2)))))))))
    (let ((list1 (tree-list-2 set1))
          (list2 (tree-list-2 set2)))
         (let ((union-list (union-list list1 list2)))
            (list->tree union-list))))

(print (union-set set1 set2))
