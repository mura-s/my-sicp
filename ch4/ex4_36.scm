(use compat.sicp)

; an-integer-betweenをan-integer-starting-fromに置き換えただけでは, 任意のPythagoras三角形を生成する方法としては適切でない理由:
;   an-integer-starting-fromは探索が失敗することがないため、一番内側のループのkの値しか変化しない。(それより上のループにバックトラックしない。)

; 本当に実現する手続き:
(define (a-pythagorean-triple-from low)
    (let ((k (an-integer-starting-from low)))
        (let ((i (an-integer-between low k)))
            (let ((j (an-integer-between i k)))
                (require (= (+ (* i i) (* j j)) (* k k)))
                (list i j k)))))
