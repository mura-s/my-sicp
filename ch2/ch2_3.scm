(use compat.sicp)

;; 2.3.1
(define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

; (print (memq 'apple '(pear banana prune)))
; (print (memq 'apple '(x (apple sauce) y apple pear)))

;; 2.3.2
(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ((sum? exp)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)))
          ((product? exp)
            (make-sum (make-product (multiplier exp)
                                    (deriv (multiplicand exp) var))
                      (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp))))
          (else
            (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

; (define (make-sum a1 a2) (list '+ a1 a2))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

; (define (make-product m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(print (deriv '(+ x 3) 'x))
(print (deriv '(* x y) 'x))
(print (deriv '(* (* x y) (+ x 3)) 'x))

;; 2.3.3
; Sets as unordered lists
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2))
            '())
          ((element-of-set? (car set1) set2)        
            (cons (car set1)
                  (intersection-set (cdr set1) set2)))
          (else
            (intersection-set (cdr set1) set2))))

; Sets as ordered lists
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()    
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                    (cons x1 (intersection-set (cdr set1)
                                               (cdr set2))))
                  ((< x1 x2)
                    (intersection-set (cdr set1) set2))
                  ((< x2 x1)
                    (intersection-set set1 (cdr set2)))))))
