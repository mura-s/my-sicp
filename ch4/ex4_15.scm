(define (run-forever) (run-forever))

(define (try p)
    (if (halts? p p)
        (run-forever)
        'halted))

; (try try) の評価を考える。
; (halts? try try) がtrue, つまり停止すると判定した場合、 (run-forever) が実行されるため、停止せず矛盾する。
; また、 (halts? try tru) がfalseの場合、 'halted となり、停止するため矛盾する。
