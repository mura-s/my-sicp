(use compat.sicp)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (tree-list-1 tree)
    (if (null? tree)
        '()
        (append (tree-list-1 (left-branch tree))
                (cons (entry tree)
                      (tree-list-1 (right-branch tree))))))

(define (tree-list-2 tree)
    (define (copy-to-list tree result-list)
        (if (null? tree)
            result-list
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree)
                                              result-list)))))
    (copy-to-list tree '()))

(print (tree-list-1
            (make-tree 7
                       (make-tree 3
                                  (make-tree 1 '() '())
                                  (make-tree 5 '() '()))
                       (make-tree 9
                                  '()
                                  (make-tree 11 '() '())))))

(print (tree-list-2
            (make-tree 7
                       (make-tree 3
                                  (make-tree 1 '() '())
                                  (make-tree 5 '() '()))
                       (make-tree 9
                                  '()
                                  (make-tree 11 '() '())))))

; a.
; tree-list-1, tree-list-2ともに中間順(inorder)探索。
; よって同じ結果を生じる。

; b.
; tree-list-1は、tree-listで木を枝まで操作していく回数がlognに比例し、
; 1階層をappendするコストがnに比例するので、O(nlogn)
; tree-list-2は、各ノードが1回ずつ訪問され、consで繋ぐだけなので、O(n)
; よって、ステップ数はtree-list-2の方が遅く増加する
