; 階乗計算機の机上シミュレーション
; n=2とする
(save continue)                       -> continue = (label fact-done)
(save n)                              -> n = 2
(assign n (op -) (reg n) (const 1))   -> n = 1
(assign continue (label after-fact))  -> continue = (label after-fact)
base-caseにjump
(assign val (const 1))                -> val = 1
after-factにjump
(restore n)                           -> n = 2
(restore continue)                    -> continue = (label fact-done)
(assign val (op *) (reg n) (reg val)) -> val = 2
fact-doneにjump

; Fibonacci計算機の机上シミュレーション
省略
