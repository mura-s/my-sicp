(use compat.sicp)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
    (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (cons new-row k))))

(define empty-board nil)

(define (last-pos k positions)
    (if (= k 1)
        (car positions)
        (last-pos (- k 1) (cdr positions))))

(define (without-last k positions)
    (if (= k 1)
        nil
        (cons (car positions) (without-last (- k 1) (cdr positions)))))

(define (safe? k positions)
    (let ((last (last-pos k positions))
          (others (without-last k positions)))
        (safe-last-pos? last others)))

(define (safe-last-pos? last others)
    (cond ((null? others)
            true)
          ((or (unsafe-row? last (car others))
               (unsafe-diagonal? last (car others)))
            false)
          (else
            (safe-last-pos? last (cdr others)))))

(define (unsafe-row? last pos)
    (= (car last) (car pos)))

(define (unsafe-diagonal? last pos)
    (let ((dx (abs (- (cdr last) (cdr pos))))
          (dy (abs (- (car last) (car pos)))))
         (= dx dy)))

(print (queens 4))
(print (queens 8))
