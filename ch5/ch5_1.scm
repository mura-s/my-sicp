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
