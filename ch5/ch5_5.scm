(use compat.sicp)

; 5.5

; 4.1.4からコピー
(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

(define (eval-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env)
                      env)
    'ok)

(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (quoted? exp)
    (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp)
    (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

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

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
(define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
    (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; else節なし
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                        clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))

(define (true? x)
    (not (eq? x false)))

(define (false? x)
    (eq? x false))

(define (make-procedure parameters body env)
    (list 'procedure parameters body env))
  
(define (compound-procedure? p)
    (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
    (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                   ((eq? var (car vars)) (car vals))
                   (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) (env-loop (enclosing-environment env)))
                   ((eq? var (car vars)) (set-car! vals val))
                   (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
                (scan (frame-variables frame)
                      (frame-values frame)))))
    (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (define (scan vars vals)
            (cond ((null? vars) (add-binding-to-frame! var val frame))
                   ((eq? var (car vars)) (set-car! vals val))
                   (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))

(define (setup-environment)
    (let ((initial-env
            (extend-environment (primitive-procedure-names)
                                (primitive-procedure-objects)
                                the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          ; ⟨基本手続きが続く⟩
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list '> >)
          (list '< <)
          (list '= =)
          (list 'list list)
          (list 'abs abs)
          ))
(define (primitive-procedure-names)
    (map car primitive-procedures))
(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
         primitive-procedures))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementation proc) args))

; 5.5.1
(define (compile exp target linkage)
    (cond ((self-evaluating? exp)
            (compile-self-evaluating exp target linkage))
          ((quoted? exp) (compile-quoted exp target linkage))
          ((variable? exp)
            (compile-variable exp target linkage))
          ((assignment? exp)
            (compile-assignment exp target linkage))
          ((definition? exp)
            (compile-definition exp target linkage))
          ((if? exp) (compile-if exp target linkage))
          ((lambda? exp) (compile-lambda exp target linkage))
          ((begin? exp)
            (compile-sequence (begin-actions exp)
                              target
                              linkage))
          ((cond? exp) (compile (cond->if exp) target linkage))
          ((application? exp)
            (compile-application exp target linkage))
          (else
            (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
    (list needs modifies statements))

(define (empty-instruction-sequence)
    (make-instruction-sequence '() '() '()))

; 5.5.2
(define (compile-linkage linkage)
    (cond ((eq? linkage 'return)
            (make-instruction-sequence
                '(continue)
                '()
                '((goto (reg continue)))))
          ((eq? linkage 'next)
            (empty-instruction-sequence))
          (else
            (make-instruction-sequence
                '()
                '()
                `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
    (preserving '(continue)
                instruction-sequence
                (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
    (end-with-linkage
        linkage
        (make-instruction-sequence
            '()
            (list target)
            `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
    (end-with-linkage
        linkage
        (make-instruction-sequence
            '()
            (list target)
            `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
    (end-with-linkage
        linkage
        (make-instruction-sequence
            '(env)
            (list target)
            `((assign ,target
                      (op lookup-variable-value)
                      (const ,exp)
                      (reg env))))))

(define (compile-assignment exp target linkage)
    (let ((var (assignment-variable exp))
          (get-value-code
            (compile (assignment-value exp) 'val 'next)))
        (end-with-linkage
            linkage
            (preserving
                '(env)
                get-value-code
                (make-instruction-sequence
                    '(env val)
                    (list target)
                    `((perform (op set-variable-value!)
                               (const ,var)
                               (reg val)
                               (reg env))
                      (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
    (let ((var (definition-variable exp))
          (get-value-code
            (compile (definition-value exp) 'val 'next)))
        (end-with-linkage
            linkage
            (preserving
                '(env)
                get-value-code
                (make-instruction-sequence
                    '(env val)
                    (list target)
                    `((perform (op define-variable!)
                               (const ,var)
                               (reg val)
                               (reg env))
                      (assign ,target (const ok))))))))

(define (compile-if exp target linkage)
    (let ((t-branch (make-label 'true-branch))
          (f-branch (make-label 'false-branch))                    
          (after-if (make-label 'after-if)))
        (let ((consequent-linkage
                (if (eq? linkage 'next) after-if linkage)))
            (let ((p-code (compile (if-predicate exp) 'val 'next))
                  (c-code
                    (compile (if-consequent exp) target consequent-linkage))
                  (a-code
                    (compile (if-alternative exp) target linkage)))
                (preserving
                    '(env continue)
                    p-code
                    (append-instruction-sequences
                        (make-instruction-sequence
                            '(val)
                            '()
                            `((test (op false?) (reg val))
                             (branch (label ,f-branch))))
                        (parallel-instruction-sequences
                            (append-instruction-sequences t-branch c-code)
                            (append-instruction-sequences f-branch a-code))
                        after-if))))))

(define (compile-sequence seq target linkage)
    (if (last-exp? seq)
        (compile (first-exp seq) target linkage)
        (preserving
            '(env continue)
            (compile (first-exp seq) target 'next)
            (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
    (let ((proc-entry (make-label 'entry))
          (after-lambda (make-label 'after-lambda)))
            (let ((lambda-linkage
                    (if (eq? linkage 'next) after-lambda linkage)))
                (append-instruction-sequences
                    (tack-on-instruction-sequence
                        (end-with-linkage lambda-linkage
                            (make-instruction-sequence '(env) (list target)
                            `((assign ,target
                             (op make-compiled-procedure)
                             (label ,proc-entry)
                             (reg env)))))
                        (compile-lambda-body exp proc-entry))
                    after-lambda))))

(define (compile-lambda-body exp proc-entry)
    (let ((formals (lambda-parameters exp)))
        (append-instruction-sequences
            (make-instruction-sequence '(env proc argl) '(env)
            `(,proc-entry
                (assign env (op compiled-procedure-env) (reg proc))
                (assign env
                        (op extend-environment)
                        (const ,formals)
                        (reg argl)
                        (reg env))))
            (compile-sequence (lambda-body exp) 'val 'return))))

(define label-counter 0)
(define (new-label-number)
    (set! label-counter (+ 1 label-counter))
    label-counter)
(define (make-label name)
    (string->symbol
        (string-append (symbol->string name)
                       (number->string (new-label-number)))))

(define (make-compiled-procedure entry env)
    (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
    (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

