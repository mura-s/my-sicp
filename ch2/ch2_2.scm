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
