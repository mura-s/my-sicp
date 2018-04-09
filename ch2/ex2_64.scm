(use compat.sicp)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

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

; a.
; リストから「真ん中の大きさの要素(this-entry)」、「それより左の要素(left-tree)」、「それより右の要素(right-tree)」と分割し、
; 木を作るという操作を再帰的に行なっていく。
;
; (1 3 5 7 9 11)に対して作る木は、
;     5
;   /   \
;  1     9
;   \   / \
;    3 7   11

(print (list->tree '(1 3 5 7 9 11)))

; b.
; 各要素に対して1度ずつmake-treeを行なっていくだけなので、O(n)
