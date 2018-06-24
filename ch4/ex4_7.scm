(use compat.sicp)

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((let*? exp) (eval (let*->nested-lets exp) env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cdr exp))
(define (let*-bindings clauses) (car clauses))
(define (let*-body clauses) (cdr clauses))

(define (make-let bindings body) (list 'let bindings body))

(define (let*->nested-lets exp) (expand-let*-clauses (let*-clauses exp)))

(define (expand-let*-clauses clauses)
    (define (iter bindings body)
        (if (null? bindings)
            body
            (make-let (list (car bindings))
                      (iter (cdr bindings) body))))
    (iter (let*-bindings clauses) (let*-body clauses)))
