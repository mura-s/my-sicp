(use compat.sicp)

(define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; else節なし
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                        clauses))
                (make-if (cond-predicate first)
                         (let ((actions (cond-actions first))
                               (predicate (cond-predicate first)))
                            (if (eq? (car actions) '=>))
                                (list (cadr actions) predicate)
                                (sequence->exp actions))
                         (expand-clauses rest))))))
