(define (make-segment p1 p2)
    (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-point x y)
    (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-rectangle h-seg w-seg) (cons h-seg w-seg))

(define (h-seg r) (car r))

(define (w-seg r) (cdr r))

(define (height r)
    (let ((ss (start-segment (h-seg r)))
          (es (end-segment (h-seg r))))
        (abs (- (y-point es) (y-point ss)))))

(define (width r)
    (let ((ss (start-segment (w-seg r)))
          (es (end-segment (w-seg r))))
        (abs (- (x-point es) (x-point ss)))))

(define (perimeter r)
    (* 2 (+ (height r) (width r))))

(define (area r)
    (* (height r) (width r)))

(define rec (make-rectangle
                (make-segment (make-point 1 1) (make-point 1 4))
                (make-segment (make-point 1 1) (make-point 5 1))))

(print (perimeter rec))
(print (area rec))

;; 違う表現の実装は省略するが、違う表現を作ってheightとwidthを実装すれば、
;; perimeterとareaを同じように使用可能。
