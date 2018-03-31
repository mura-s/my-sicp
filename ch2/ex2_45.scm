(define (split sp1 sp2)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split sp1 sp2) painter (- n 1))))
                (sp1 painter (sp2 smaller smaller))))))
