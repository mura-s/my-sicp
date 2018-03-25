(use compat.sicp)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
    (flatmap (lambda (i)
                (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval 1 (- j 1))))
                         (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (eq-sum? l s)
    (= s (+ (car l) (cadr l) (caddr l))))

(define (make-list-sum l)
    (list (car l)
          (cadr l)
          (caddr l)
          (+ (car l) (cadr l) (caddr l))))

(define (eq-s-sum-list n s)
    (map make-list-sum
         (filter (lambda (l) (eq-sum? l s))
                 (unique-pairs n))))

(print (eq-s-sum-list 6 10))
(print (eq-s-sum-list 6 12))
