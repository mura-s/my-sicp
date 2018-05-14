(define x 10)

(parallel-execute
    (lambda () (set! x (* x x)))     ; P1
    (lambda () (set! x (* x x x))))  ; P2

; 全ての可能性:
;   1,000,000: P1後にP2
;   1,000,000: P2後にP1
;   100: 10の時点でP1が読み込み、最終的にP1が書き込む
;   10,000: 10と1000をP1が読み込み、書き込む
;   1,000: 10の時点でP2が読み込み、最終的にP2が書き込む
;   100,000: P2が10, 100, 100を読み込み、書き込む
;   10,000: P2が10, 10, 100を読み込む、書き込む

(define x 10)
(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; どれが残るか:
;   1,000,000: P1後にP2
;   1,000,000: P2後にP1
