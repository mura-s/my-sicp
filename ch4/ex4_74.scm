; a.
(define (simple-stream-flatmap proc s)
    (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
    (stream-map stream-car
                (stream-filter (lambda (s) (not (stream-null? s))) stream)))

; negateの実装などを参照

; b.
結果は変わらない。
