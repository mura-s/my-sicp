(use compat.sicp)

; a.
(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((letrec? exp) (eval (letrec->let exp) env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-clauses exp) (cdr exp))
(define (letrec-bindings clauses) (car clauses))
(define (letrec-body clauses) (cdr clauses))
(define (letrec-vars bindings) (map car bindings))
(define (letrec-exps bindings) (map cadr bindings))

(define (letrec->let exp)
    (expand-letrec-clauses (letrec-clauses exp)))

(define (expand-letrec-clauses clauses)
    (let ((vars (letrec-vars (letrec-bindings clauses)))
          (exps (letrec-exps (letrec-bindings clauses)))
          (body (letrec-body clauses)))
        (cons 'let
              (cons (map (lambda (x) (list x ''*unassigned*)) vars)
                    (append (map (lambda (x y) (list 'set! x y)) vars exps)
                            body)))))

(print (letrec->let '(letrec ((fact (lambda (n)
                                        (if (= n 1)
                                        1
                                        (* n (fact (- n 1)))))))
                            (fact 10))))

; b.
; defineをそのままletに置き換えようとした場合に再帰的な定義を使うことができない。
