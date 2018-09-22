(use compat.sicp)

; 5.1
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

; 5.1.1
; (controller
;     test-b
;         (test (op =) (reg b) (const 0))
;         (branch (label gcd-done))
;         (assign t (op rem) (reg a) (reg b))
;         (assign a (reg b))
;         (assign b (reg t))
;         (goto (label test-b))
;     gcd-done)

; (controller
;     gcd-loop
;         (assign a (op read))
;         (assign b (op read))
;     test-b
;         (test (op =) (reg b) (const 0))
;         (branch (label gcd-done))
;         (assign t (op rem) (reg a) (reg b))
;         (assign a (reg b))
;         (assign b (reg t))
;         (goto (label test-b))
;     gcd-done
;         (perform (op print) (reg a))
;         (goto (label gcd-loop)))

; 5.1.2
(define (remainder n d)
    (if (< n d)
        n
        (remainder (- n d) d)))

; (controller
;     test-b
;         (test (op =) (reg b) (const 0))
;         (branch (label gcd-done))
;         (assign t (reg a))
;     rem-loop
;         (test (op <) (reg t) (reg b))
;         (branch (label rem-done))
;         (assign t (op -) (reg t) (reg b))
;         (goto (label rem-loop))
;     rem-done
;         (assign a (reg b))
;         (assign b (reg t))
;         (goto (label test-b))
;     gcd-done)

; 5.1.3
; gcd
;     (test (op =) (reg b) (const 0))
;     (branch (label gcd-done))
;     (assign t (op rem) (reg a) (reg b))
;     (assign a (reg b))
;     (assign b (reg t))
;     (goto (label gcd))
; gcd-done
;     (goto (reg continue))
  
;     ;; gcdを呼び出す前に, gcdから戻るべきラベルを
;     ;; continueに代入する.
;     (assign continue (label after-gcd-1))
;     (goto (label gcd))
; after-gcd-1
  
;     ;; gcdへの第二の呼出しでは継続は異る.
;     (assign continue (label after-gcd-2))
;     (goto (label gcd))
; after-gcd-2

; 5.1.4
; (controller
;         (assign continue (label fact-done))     ; 最終帰り番地設定
;     fact-loop
;         (test (op =) (reg n) (const 1))
;         (branch (label base-case))
;         ;; nとcontinueを退避し再帰呼出しを設定する.
;         ;; 再帰呼出しから戻る時after-factから
;         ;; 計算が続行するようにcontinueを設定
;         (save continue)
;         (save n)
;         (assign n (op -) (reg n) (const 1))
;         (assign continue (label after-fact))
;         (goto (label fact-loop))
;     after-fact
;         (restore n)
;         (restore continue)
;         (assign val (op *) (reg n) (reg val))   ; valに n(n-1)!がある
;         (goto (reg continue))                   ; 呼出し側に戻る
;     base-case
;         (assign val (const 1))                  ; 基底の場合: 1!=1
;         (goto (reg continue))                   ; 呼出し側に戻る
;     fact-done)

; (controller
;         (assign continue (label fib-done))
;     fib-loop
;         (test (op <) (reg n) (const 2))
;         (branch (label immediate-answer))
;         ;; Fib(n-1)を計算するよう設定
;         (save continue)
;         (assign continue (label afterfib-n-1))
;         (save n)                           ; nの昔の値を退避
;         (assign n (op -) (reg n) (const 1)); nを n-1 に変える
;         (goto (label fib-loop))            ; 再帰呼出しを実行
;     afterfib-n-1                         ; 戻った時 Fib(n-1)はvalにある
;         (restore n)
;         (restore continue)
;         ;; Fib(n-2)を計算するよう設定
;         (assign n (op -) (reg n) (const 2))
;         (save continue)
;         (assign continue (label afterfib-n-2))
;         (save val)                         ; Fib(n-1)を退避
;         (goto (label fib-loop))
;     afterfib-n-2                         ; 戻った時Fib(n-2)の値はvalにある
;         (assign n (reg val))               ; nにはFib(n-2)がある
;         (restore val)                      ; valにはFib(n-1)がある
;         (restore continue)
;         (assign val                        ; Fib(n-1)+Fib(n-2)
;                 (op +) (reg val) (reg n))
;         (goto (reg continue))              ; 呼出し側に戻る. 答えはvalにある
;     immediate-answer
;         (assign val (reg n))               ; 基底の場合: Fib(n)=n
;         (goto (reg continue))
;     fib-done)
