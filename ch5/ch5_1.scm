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
