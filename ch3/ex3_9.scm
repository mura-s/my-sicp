(use compat.sicp)

; 再帰版
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

(factorial 6)
; 環境構造:
; - factorialが大域環境に定義される
; - (factorial 6) を実行すると、大域環境へのポインタを持つ新しいフレームを作成し、n=6を束縛する
; - その中で (factorial (- n 1)) が実行され、また大域環境へのポインタを持つ新しいフレームを作成され、今度はn=5を束縛する
; - これを繰り返し実行していく

; 反復版
(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

(factorial 6)
; 環境構造:
; - factorialとfact-iterが大域環境に定義される
; - (factorial 6) を実行すると、大域環境へのポインタを持つ新しいフレームを作成し、n=6を束縛する
; - (fact-iter 1 1 n) が実行され、大域環境へのポインタを持つ新しいフレームを作成し、product=1, counter=1, max-count=6を束縛する
; - その中で (fact-iter ...) が実行され、大域環境へのポインタを持つ新しいフレームを作成され、今度はproduct=1, counter=2, max-count=5を束縛する
; - これを繰り返し実行していく
