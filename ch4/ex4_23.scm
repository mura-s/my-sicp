(use compat.sicp)

(define (analyze-sequence exps)
    (define (execute-sequence procs env)
        (cond ((null? (cdr procs)) ((car procs) env))
              (else ((car procs) env)
                    (execute-sequence (cdr procs) env))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE"))
        (lambda (env) (execute-sequence procs env))))

; - 並びが一つの場合
;   - Alyssa版: lambda式が返される
;   - 本文版: 並び自体(proc自体)が返される

; - 並びが二つの場合
;   - Alyssa版: lambda式が返される。なお、procsは実行時に再帰的に展開されていく。
;   - 本文版: procsが全て展開された後のlambda式が返される。

; つまり、Alyssa版は実行時にprocsの再帰的な展開が必要なため効率が悪い。
