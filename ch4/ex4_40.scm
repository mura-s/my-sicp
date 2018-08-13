; 人の階への割当ての組は, 階の割当てが相異るという要求をする前と後では, いくつか:
;   前: 5^5 = 3125
;   後: 5! = 120

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4)))
        (let ((cooper (amb 2 3 4 5)))
            (let ((fletcher (amb 2 3 4)))
                (require (not (= (abs (- fletcher cooper)) 1)))
                (let ((miller (amb 3 4 5)))
                    (require (> miller cooper))
                    (let ((smith (amb 1 2 3 4 5)))
                        (require (not (= (abs (- smith fletcher)) 1)))
                        (require (distinct? (list baker cooper fletcher miller smith)))
                        (list (list 'baker baker)
                              (list 'cooper cooper)
                              (list 'fletcher fletcher)
                              (list 'miller miller)
                              (list 'smith smith))))))))
