(use compat.sicp)

(define (make-table)
    ; tree
    (define (key-of-tree tree) (car tree))
    (define (entry-of-tree tree) (cadr tree))
    (define (left-branch tree) (caddr tree))
    (define (right-branch tree) (cadddr tree))
    (define (make-tree key entry left right)
        (list key entry left right))

    (define (adjoin-tree key value tree)
        (cond ((null? tree) (make-tree key value '() '()))
              ((= key (key-of-tree tree))
                (make-tree key
                           value
                           (left-branch tree)
                           (right-branch tree)))
              ((< key (key-of-tree tree))
                (make-tree (key-of-tree tree) 
                           (entry-of-tree tree)
                           (adjoin-tree key value (left-branch tree))
                           (right-branch tree)))
              ((> key (key-of-tree tree))
                (make-tree (key-of-tree tree)
                           (entry-of-tree tree)
                           (left-branch tree)
                           (adjoin-tree key value (right-branch tree))))))

    (define (lookup-tree given-key tree)
        (cond ((null? tree) false)
              ((= given-key (key-of-tree tree))
                (entry-of-tree tree))
              ((< given-key (key-of-tree tree))
                (lookup-tree given-key (left-branch tree)))
              ((> given-key (key-of-tree tree))
                (lookup-tree given-key (right-branch tree)))))

    ; table
    (let ((local-table (cons '*table* '())))
        (define (lookup key)
            (lookup-tree key (cdr local-table)))

        (define (insert! key value)
            (set-cdr! local-table
                      (adjoin-tree key value (cdr local-table)))
            'ok)

        (define (get-table) local-table)

        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  ((eq? m 'get-table-proc) get-table)
                  (else (error "Unknown operation -- TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define get-table (operation-table 'get-table-proc))

(put 5 'hoge)
(put 2 'foo)
(put 3 'baz)
(put 7 'bar)
(print (get-table))
(print (get 7))
(print (get 9))
