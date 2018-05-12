(use compat.sicp)

(define (or-gate o1 o2 output)
    (let ((a (make-wire))
          (b (make-wire))
          (c (make-wire)))
        (inverter o1 a)
        (inverter o2 b)
        (and-gate a b c)
        (inverter c output)
        'ok))

; 遅延は、 and-gate-delay + inverter-delay * 2
