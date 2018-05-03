(use compat.sicp)

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch)

(define acc (make-account 50))

; ((acc 'deposit) 40)
; 90
; ((acc 'withdraw) 60)
; 30

; 環境構造:
;   make-accountがbalanceや内部手続きを持つ環境を作成する.
;   accは大域環境に定義される.

; accの内部状態はどこに保持されているか:
;   make-accountが作り出した環境の中に保持されている.

(define acc2 (make-account 100))

; 二つの口座の局所状態はどのようにして別々に保たれるか. accとacc2で, 共有する環境構造はどの部分か:
;    make-accountの呼び出しごとに別々の環境が作成されるので別々に保たれる.
;    共有している環境構造は、大域環境.
