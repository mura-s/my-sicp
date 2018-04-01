(use compat.sicp)

;; 2.3.1
(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

; (print (memq 'apple '(pear banana prune)))
; (print (memq 'apple '(x (apple sauce) y apple pear)))
