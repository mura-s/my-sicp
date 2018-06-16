(use compat.sicp)

; a.
;  優先順位が変わってしまうため、例えば、 (define x 3) に対して eval-definition を実行するのではなく、
;  apply を実行してしまう。
;  その結果、(eval (operator exp) env) で lookup-variable-value が呼ばれ、
;  define が未定義のためエラーになってしまう。

; b.
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
