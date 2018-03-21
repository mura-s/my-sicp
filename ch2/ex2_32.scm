(use compat.sicp)

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest
                    (map (lambda (r) (cons (car s) r))
                         rest)))))

(print (subsets (list 1 2 3)))

; subsetsでは集合(1 2 3)について、以下のような処理を行っている。
; 1. まず集合の最後の要素であるnilを (()) という形で返す。
; 2. 1.の結果の部分集合に対して、先頭に3をappendしたものを結果の集合に加えて返す。(結果は (() (3)) )
; 3. 結果の各要素に対して、先頭に2をappendしたものを結果の集合に加えて返す。(結果は (() (3) (2) (2 3)) )
; 4. 3.と同じことを1に対しても行うと解答が得られる。
; つまり、結果の部分集合に対して、次の値を加えたときの全ての組み合わせを網羅していくので、上手くいく。
