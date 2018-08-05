(use compat.sicp)

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))
(define (definition? exp)
    (tagged-list? exp 'define))
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)   ; 仮パラメタ
                     (cddr exp)))) ; 本体

; a.
(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                  ((eq? var (car vars)) 
                    (if (eq? (car vals) '*unassigned*)
                        (error "Unassigned variable" var)
                        (car vals)))
                  (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))

; b.
(define (scan-out-defines body)
    (define (iter exp vars sets exps)
        (if (null? exp)
            (list (reverse vars) (reverse sets) (reverse exps))
            (if (definition? (car exp))
                (iter (cdr exp)
                      (cons (list (definition-variable (car exp)) ''*unassigned*) vars)
                      (cons (list 'set! (definition-variable (car exp)) (definition-value (car exp))) sets)
                      exps)
                (iter (cdr exp)
                      vars
                      sets
                      (cons (car exp) exps)))))
    (define (include-define? exp)
        (if (null? exp)
            false
            (if (definition? (car exp))
                true
                (include-define? (cdr exp)))))
    (if (include-define? body)
        (let ((var-val-exp-list (iter body '() '() '())))
            (list (cons 'let
                        (cons (car var-val-exp-list)
                              (append (cadr var-val-exp-list) (caddr var-val-exp-list))))))
        body))

(print (scan-out-defines
        '((define u 3)
          (define v 5)
          (+ u v))))

; c.
; make-procedureに組み込んだほうがいい。
; procedure-bodyは選択子なので何回も呼び出される可能性があるため。
