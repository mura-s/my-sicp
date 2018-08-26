(use compat.sicp)

(define (analyze exp)
    (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
          ; 略
          ((let? exp) (analyze (let->combination exp)))
          ; 略
          (else (error "Unknown expression type -- ANALYZE" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cdr exp))
(define (let-bindings clauses) (car clauses))
(define (let-body clauses) (cdr clauses))

(define (let-vars bindings) (map car bindings))
(define (let-exps bindings) (map cadr bindings))

(define (let->combination exp) (expand-let-clauses (let-clauses exp)))

(define (expand-let-clauses clauses)
    (cons (make-lambda (let-vars (let-bindings clauses))
                       (let-body clauses))
          (let-exps (let-bindings clauses))))
