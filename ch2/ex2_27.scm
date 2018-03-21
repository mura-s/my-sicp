(use compat.sicp)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
    (define (iter a rev)
        (if (null? a)
            rev
            (iter (cdr a) (cons (car a) rev))))
    (iter items nil))

(define x (list (list 1 2) (list 3 4)))

(print (reverse x))

(define (deep-reverse items)
    (if (pair? items)
        (append (deep-reverse (cdr items))
                (list (deep-reverse (car items))))
        items))

(print (deep-reverse x))
