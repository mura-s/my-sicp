(define (unique-query exps) (car exps))

(define (uniquely-asserted operands frame-stream)
    (stream-flatmap
        (lambda (frame)
                (let ((result (qeval (unique-query operands)
                                     (singleton-stream frame))))
                    (if (and (not (stream-null? result))
                             (stream-null? (stream-cdr result)))
                        result ;; 唯一個の結果を持つストリームの場合
                        the-empty-stream))) ;; 結果が無いか、2個以上の結果を持つ場合
        frame-stream))

(put 'unique 'qeval uniquely-asserted)


; 唯一人を監督する人すべてをリストする質問 (REPL上で実行)
(and (supervisor ?x ?j)
     (unique (supervisor ?anyone ?j)))
