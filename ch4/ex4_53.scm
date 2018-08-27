; (let ((pairs '()))
;   (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
;              (permanent-set! pairs (cons p pairs))
;              (amb))
;            pairs))

; pairsに素数となる組のリストが追加されて返ってくる。
