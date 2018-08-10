(use compat.sicp)

(define (unless condition usual-value exceptional-value)
    (if condition exceptional-value usual-value))

(define (factorial n)
    (unless (= n 1)
        (* n (factorial (- n 1)))
        1))

(factorial 5)

; 作用的順序の場合: 無限ループになる。

; 正規順序の場合: unlessの評価時には引数は評価されないため、無限ループにならず、正常に終了する。
