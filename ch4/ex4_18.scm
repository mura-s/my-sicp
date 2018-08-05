(use compat.sicp)

; 元の式.
; (lambda ⟨vars⟩
;     (define u ⟨e1⟩)
;     (define v ⟨e2⟩)
;     ⟨e3⟩)

; solve手続き.
; (define (solve f y0 dt)
;     (define y (integral (delay dy) y0 dt))
;     (define dy (stream-map f y))
;     y)

; 変換1.
; (lambda ⟨vars⟩
;     (let ((u '*unassigned*)
;           (v '*unassigned*))
;       (set! u ⟨e1⟩)
;       (set! v ⟨e2⟩)
;       ⟨e3⟩))

; 変換2.
; (lambda ⟨vars⟩
;     (let ((u '*unassigned*)
;           (v '*unassigned*))
;       (let ((a ⟨e1⟩)
;             (b ⟨e2⟩))
;         (set! u a)
;         (set! v b))
;       ⟨e3⟩))

; 変換2. は動かない。 solveでは、e2が実行される際にuを参照するが、このタイミングではuはunassigned*のため。
; 変換1. は動く。
