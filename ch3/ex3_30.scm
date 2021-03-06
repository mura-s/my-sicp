(use compat.sicp)

(define (ripple-carry-adder as bs ss c)
    (define (iter as bs ss cin)
        (let ((a (car as))
              (b (car bs))
              (s (car ss))
              (at (cdr as))
              (bt (cdr bs))
              (st (cdr ss)))
            (if (and (null? at) (null? bt) (null? st))
                (full-adder a b cin s c)
                (let ((cout (make-wire)))
                    (full-adder a b cin s cout)
                    (iter at bt st cout)))))
    (let ((c0 (make-wire)))
        (set-signal! c0 0)
        (iter as bs ss c0)))

; 遅延:
;   n * (full-adder-carry-delay)
;   = n * (half-adder-sum-delay + and-gate-delay + or-gate-delay)
;   = n * (((max (or-gate-delay) (and-gate-delay + inverter-delay)) + and-gate-delay) + and-gate-delay + or-gate-delay)
