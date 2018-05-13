(set! balance (+ balance 10))
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))

; a.
; Peter -> Paul -> Mary: 45
; Peter -> Mary -> Paul: 35
; Paul -> Peter -> Mary: 45
; Paul -> Mary -> Peter: 50
; Mary -> Peter -> Paul: 40
; Mary -> Paul -> Peter: 40

; b.
; 例えば、110のような値が生じうる。
; これは同時にbalance=100を読み込み、最後にPeterの結果が書き込まれた場合。 
