(use compat.sicp)

(define (list-of-values-lr exps env)
    (if (no-operands? exps)
        '()
        (let ((first (eval (first-operand exps) env)))
            (cons first
                  (rest (list-of-values (rest-operands exps) env))))

(define (list-of-values-rl exps env)
    (if (no-operands? exps)
        '()
        (let ((rest (list-of-values (rest-operands exps) env)))
            (cons (first (eval (first-operand exps) env))
                  rest))
