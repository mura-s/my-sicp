(use compat.sicp)

(define (square-list1 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (square (car things))
                        answer))))
    (iter items nil))

(print (square-list1 (list 1 2 3 4)))
; thingsから取り出した値を、順番にanswerの先頭に入れていくため、逆順になってしまう.

(define (square-list2 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer 
                        (square (car things))))))
    (iter items nil))

(print (square-list2 (list 1 2 3 4)))
; iterごとに、(answer . (square (car things))) を作ってanswerに渡す形になっており、
; answerの階層が深くなっていく処理になっているため、正しくない.
