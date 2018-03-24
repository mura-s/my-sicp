(use compat.sicp)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op
                          init
                          (map (lambda (ss) (car ss)) seqs))
              (accumulate-n op
                            init
                            (map (lambda (ss) (cdr ss)) seqs)))))

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(print (accumulate-n + 0 seqs))
