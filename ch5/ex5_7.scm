; a. 再帰的べき乗:
(define expt-rec-machine
    (make-machine
        '(continue n val b)
        (list (list '= =) (list '- -) (list '* *))
        '((assign continue (label expt-done))
    expt-loop
        (test (op =) (reg n) (const 0))        
        (branch (label base-case))
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label expt-loop))
    after-fact
        (restore continue)
        (assign val (op *) (reg val) (reg b))
        (goto (reg continue))
    base-case
        (assign val (const 1))
        (goto (reg continue))
    expt-done)))

; b. 反復的べき乗:
(define expt-iter-machine
    (make-machine
        '(counter n b product)
        (list (list '= =) (list '- -) (list '* *))
        '((assign counter (reg n))
        (assign product (const 1))
    expt-loop
        (test (op =) (reg counter) (const 0))
        (branch (label expt-done))
        (assign counter (op -) (reg counter) (const 1))
        (assign product (op *) (reg b) (reg product))
        (goto (label expt-loop))
    expt-done)))
