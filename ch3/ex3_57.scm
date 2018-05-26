(use compat.sicp)

(define fibs
    (cons-stream 0
                (cons-stream 1
                             (add-streams (stream-cdr fibs)
                                          fibs))))

; n番目のFibonacci数を計算する時, 加算は何回実行されるか:
;   (n-1)回
;   0, 1は加算が必要なく、それ以降1回ずつ加算が必要なため

; メモ化を使わない場合:
;   f(n) = f(n-1) + f(n-2) より、加算は指数的に増えていく
