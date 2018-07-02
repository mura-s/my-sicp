(use compat.sicp)

(define (make-frame variables values)
    (if (null? variables)
        '()
        (cons (cons (car variables) (car values))
              (make-frame (cdr variables) (cdr values)))))

(define (frame-variables frame)
    (if (null? frame)
        '()
        (cons (caar frame)
              (frame-variables (cdr frame)))))

(define (frame-values frame)
    (if (null? frame)
        '()
        (cons (cdar frame)
              (frame-variables (cdr frame)))))

(define (add-binding-to-frame! var val frame)
    (set! frame (cons (cons var val) frame)))
