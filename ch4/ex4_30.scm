(use compat.sicp)

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (actual-value (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

; a.
; displayは基本手続きであるため、xがforce-itされ、正しく実行される。

; b.
; 元々のeval-sequence:
;   - (p1 1): (1 2)
;   - (p2 1): 1
; eval-sequenceでevalされるが、eやxはvariable?なので、env内の値 (thunk) が返ってくる。
;
; Cyのeval-sequence:
;   - (p1 1): (1 2)
;   - (p2 1): (1 2)
; eval-sequenceでthunkがforce-itされる。

; c.
; a.の例は、たまたま基本関数 display を使っているので、eval で評価したときも遅延しない。
; Cyのように、actual-value で評価した場合はもちろん遅延しない、どちらにしても遅延しないので結果は同じになる。

; d.
; 本文中の方法の方が良いと思う。
; Cyの方法は自然な値が返るように見えるところは良いが、途中式の値が遅延されずに評価されてしまうため。
