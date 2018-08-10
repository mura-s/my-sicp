; メモ化しないと, メモ化するより遥かに遅く走ると思われるプログラムの例
(define (fib i) 
    (if (<= i 2) 
        1 
        (+ (fib (- i 1)) (fib (- i 2))))) 

(fib 25)

; 対話について
; メモ化したとき:
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100
; 
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 1

; メモ化しないとき:
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100
; 
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 2

; ref. http://www.serendip.ws/archives/2228
; timeで囲めば速度を計測できる
