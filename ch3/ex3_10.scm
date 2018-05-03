(use compat.sicp)

(define (make-withdraw initial-amount)
    (let ((balance initial-amount))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                        balance)
                "Insufficient funds"))))

; balanceを束縛する環境を作る際に、lambda式 (let式) を評価して作る部分が違うだけで、
; 他は本に図示されている図と同じになる
