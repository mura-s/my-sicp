(use compat.sicp)

(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

;; a.
(define (left-branch m)
    (car m))

(define (right-branch m)
    (car (cdr m)))

(define (branch-length b)
    (car b))

(define (branch-structure b)
    (car (cdr b)))

(define b1 (make-branch 2 3))
(define b2 (make-branch 4 5))
(define m1 (make-mobile b1 b2))

(print (left-branch m1))
(print (right-branch m1))
(print (branch-length b1))
(print (branch-structure b1))

;; b.
(define (total-weigth m)
    (let ((ls (branch-structure (left-branch m)))
          (rs (branch-structure (right-branch m))))
        (+ (if (pair? ls) (total-weigth ls) ls)
           (if (pair? rs) (total-weigth rs) rs))))

(define b3 (make-branch 1 m1))
(define m2 (make-mobile b1 b3))

(print (total-weigth m1))
(print (total-weigth m2))

;; c.
(define (brance-torque b)
    (let ((bl (branch-length b))
          (bs (branch-structure b)))
        (if (pair? bs)
            (* bl (total-weigth bs))
            (* bl bs))))
    
(define (brance-torque-eq? b1 b2)
    (= (brance-torque b1) (brance-torque b2)))

(define (brance-balanced? b1 b2)
    (let ((b1s (branch-structure b1))
          (b2s (branch-structure b2)))
         (and (brance-torque-eq? b1 b2)
              (if (pair? b1s) (balanced? b1s) true)
              (if (pair? b2s) (balanced? b2s) true))))

(define (balanced? m)
    (brance-balanced? (left-branch m) (right-branch m)))

(define m3 (make-mobile b1 b1))
(print (balanced? m1))
(print (balanced? m3))

;; d.
; 以下のデータの取り出し方の手続きのみ修正すれば良い. 
; (define (left-branch m)
;     (car m))
; (define (right-branch m)
;     (car (cdr m)))
; (define (branch-length b)
;     (car b))
; (define (branch-structure b)
;     (car (cdr b)))
