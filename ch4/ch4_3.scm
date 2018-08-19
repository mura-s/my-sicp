(use compat.sicp)

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

; 4.3.1
(define (require p)
    (if (not p) (amb)))

(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1))))

(define (prime-sum-pair list1 list2)
    (let ((a (an-element-of list1))
          (b (an-element-of list2)))
        (require (prime? (+ a b)))
        (list a b)))

; 4.3.2
; Logic Puzzles
(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))

; Parsing natural language
