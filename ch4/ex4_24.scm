; 例えば、以下の式をREPLで実行して比較してみる。
; (primitive-procedures に <= を定義する必要がある)
;
; (define (fib n) 
;     (if (<= n 2) 
;         1 
;         (+ (fib (- n 1)) (fib (- n 2))))) 
;  
; (fib 30) 
