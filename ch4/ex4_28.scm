(define (f x) (+ x 1))
(define (g proc) (proc 10))
(g f)

; 上記のようなケースで、f (= proc) はthunkであるため、強制が必要だが、
; evalの場合強制されない。
