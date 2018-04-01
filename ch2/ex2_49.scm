(define (make-vect xcor ycor)
    (cons xcor ycor))

(define (xcor-vect vect)
    (car vect))

(define (ycor-vect vect)
    (cdr vect))

(define (make-segment v1 v2)
    (cons v1 v2))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment)
                (draw-line
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
            segment-list)))

(define bl (make-vect 0 0))
(define br (make-vect 1 0))
(define tl (make-vect 0 1))
(define tr (make-vect 1 1))

(define b (make-segment 0.5 0))
(define l (make-segment 0 0.5))
(define t (make-segment 0.5 1))
(define r (make-segment 1 0.5))

;; a.
(define outline-painter
    (segments->painter
        (list (make-segment bl br)
              (make-segment br tr)
              (make-segment tr tl)
              (make-segment tl bl))))

;; b.
(define x-painter
    (segments->painter
        (list (make-segment bl tr)
              (make-segment br tl))))

;; c.
(define diamond-painter
    (segments->painter
        (list (make-segment b r)
              (make-segment r t)
              (make-segment t l)
              (make-segment l b))))

;; d.
;; 回答は略
;; ref. http://community.schemewiki.org/?sicp-ex-2.49
