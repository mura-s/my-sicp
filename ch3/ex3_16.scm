(use compat.sicp)

(define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))

; 以下のように3つの対でできているリストでも3以外の値を返すことがあるので正しくない.

; 3を返すもの
(define x1 (cons 'a 'b))
(define x2 (cons 'c 'd))
(define return3 (cons x1 x2))
(print (count-pairs return3))

; 4を返すもの
(define y1 (cons 'a 'b))
(define y2 (cons 'c y1))
(define return4 (cons y1 y2))
(print (count-pairs return4))

; 7を返すもの
(define z1 (cons 'a 'b))
(define z2 (cons z1 z1))
(define return7 (cons z2 z2))
(print (count-pairs return7))
