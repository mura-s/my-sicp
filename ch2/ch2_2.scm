(use compat.sicp)

;; 2.2.1
(define one-through-four (list 1 2 3 4))

; (print (car one-through-four))
; (print (cdr one-through-four))
; (print (cadr one-through-four))

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

; (print (list-ref squares 3))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

; (print (length odds))

(define (length-iter items)
    (define (iter a count)
        (if (null? a)
            count
            (iter (cdr a) (+ count 1))))
    (iter items 0))

; (print (length-iter odds))

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

; (print (append squares odds))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

; (print (map abs (list -10 2.5 -11.6 17)))

(define (scale-list items factor)
    (map (lambda (x) (* x factor))
         items))

; (print (scale-list (list 1 2 3 4) 10))

;; 2.2.2
(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

; (print (count-leaves (list x x)))

(define (scale-tree tree factor)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree factor))
          (else (cons (scale-tree (car tree) factor)
                      (scale-tree (cdr tree) factor)))))

; (define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (print (scale-tree t1 2))

(define (scale-tree2 tree factor)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree2 sub-tree factor)
                (* sub-tree factor)))
         tree))

; (print (scale-tree2 t1 2))

;; 2.2.3
(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree))
            (if (odd? tree) (square tree) 0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))

; (define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (print (sum-odd-squares t1))

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (even-fibs n)
    (define (next k)
        (if (> k n)
            nil
            (let ((f (fib k)))
                (if (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))))))
    (next 0))

; (print (even-fibs 5))

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

; (print (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

; (print (accumulate + 0 (list 1 2 3 4 5)))
; (print (accumulate * 1 (list 1 2 3 4 5)))
; (print (accumulate cons nil (list 1 2 3 4 5)))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

; (print (enumerate-interval 2 7))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

; (print (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(define (sum-odd-squares2 tree)
    (accumulate +
                0
                (map square
                     (filter odd?
                             (enumerate-tree tree)))))

; (define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (print (sum-odd-squares2 t1))

(define (even-fibs2 n)
    (accumulate cons
                nil
                (filter even?
                        (map fib
                             (enumerate-interval 0 n)))))
; (print (even-fibs2 5))

(define (list-fib-squares n)
    (accumulate cons
                nil
                (map square
                    (map fib
                         (enumerate-interval 0 n)))))

; (print (list-fib-squares 10))

(define (product-of-squares-of-odd-elements sequence)
    (accumulate *
                1
                (map square
                     (filter odd? sequence))))

; (print (product-of-squares-of-odd-elements (list 1 2 3 4 5)))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

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

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap (lambda (i)
                        (map (lambda (j) (list i j))
                             (enumerate-interval 1 (- i 1))))
                     (enumerate-interval 1 n)))))

; (print (prime-sum-pairs 6))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

(define (permutations s)
    (if (null? s)
        (list nil)
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x s))))
                 s)))

; (print (permutations (list 1 2 3)))

;; 2.2.4

(define (flipped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter)))
        (below painter2 painter2))))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
            (let ((top-left (beside up up))
                  (bottom-right (below right right))
                  (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(define (spuare-limit painter n)
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
              (bottom (beside (bl painter) (br painter))))
            (below bottom top))))

(define (flipped-pairs painter)
    (let ((combine4 (square-of-four identity flip-vert
                                    identity flip-vert)))
        (combine4 painter)))

(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity
                                    rotate180 flip-vert)))
        (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect
            (origin-frame frame)
            (add-vect (scale-vect (xcor-vect v)
                                  (edge1-frame frame))
                      (scale-vect (ycor-vect v)
                                  (edge2-frame frame))))))
