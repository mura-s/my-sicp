(use compat.sicp)

(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cdr exp))
(define (let-bindings clauses) (car clauses))
(define (let-body clauses) (cdr clauses))

(define (let-vars bindings) (map car bindings))
(define (let-exps bindings) (map cadr bindings))

(define (let->combination exp)
    (if (named-let? exp)
        (expand-named-let-clauses (let-clauses exp))
        (expand-let-clauses (let-clauses exp))))

(define (expand-let-clauses clauses)
    (cons (make-lambda (let-vars (let-bindings clauses))
                       (let-body clauses))
          (let-exps (let-bindings clauses))))

(define (named-let? exp)
    (not (pair? (car (let-clauses exp)))))

(define (named-let-var clauses) (car clauses))
(define (named-let-bindings clauses) (cadr clauses))
(define (named-let-body clauses) (caddr clauses))

(define (expand-named-let-clauses clauses)
    (make-begin
        (list
            (list
                'define
                (cons (named-let-var clauses) (let-vars (named-let-bindings clauses)))
                (named-let-body clauses))
            (cons (named-let-var clauses) (let-exps (named-let-bindings clauses))))))
