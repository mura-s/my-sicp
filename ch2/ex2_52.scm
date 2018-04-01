;; a.
(define wave 
    (segments->painter
        (list 
            ;; ...
            (make-segment (make-vect 0.50 0.75) (make-vect 0.42 0.78))
            (make-segment (make-vect 0.50 0.75) (make-vect 0.58 0.78))))) 

;; b.
(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1)))
              (corner (corner-split painter (- n 1))))
                (beside (below painter up)
                        (below right corner)))))

;; c.
(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
        (combine4 (corner-split (flip-horiz painter) n))))
