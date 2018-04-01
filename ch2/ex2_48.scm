(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (caddr frame))

(define (make-segment v1 v2)
    (cons v1 v2))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))
