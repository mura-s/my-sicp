(use compat.sicp)

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

; 4.3.1
(define (require p)
    (if (not p) (amb)))

(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1))))

(define (prime-sum-pair list1 list2)
    (let ((a (an-element-of list1))
          (b (an-element-of list2)))
        (require (prime? (+ a b)))
        (list a b)))

; 4.3.2
; Logic Puzzles
(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
        (require (distinct? (list baker cooper fletcher miller smith)))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (> miller cooper))
        (require (not (= (abs (- smith fletcher)) 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))

; Parsing natural language

; (define (parse-sentence)
;     (list 'sentence
;           (parse-noun-phrase)
;           (parse-word verbs)))

; (define (parse-noun-phrase)
;     (list 'noun-phrase
;           (parse-word articles)
;           (parse-word nouns)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define *unparsed* '())

(define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
        (require (null? *unparsed*))
        sent))

(define (parse-sentence)
    (list 'sentence
          (parse-noun-phrase)
          (parse-verb-phrase)))

(define (parse-noun-phrase)
    (define (maybe-extend noun-phrase)
        (amb noun-phrase
            (maybe-extend (list 'noun-phrase
                                noun-phrase
                                (parse-prepositional-phrase)))))
    (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns)))

(define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
        (amb verb-phrase
            (maybe-extend (list 'verb-phrase
                                verb-phrase
                                (parse-prepositional-phrase)))))
    (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
    (require (not (null? *unparsed*)))
    (require (memq (car *unparsed*) (cdr word-list)))
    (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
    (list 'prep-phrase
          (parse-word prepositions)
          (parse-noun-phrase)))

; 4.3.3
(define apply-in-underlying-scheme apply)

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
          (list 'list list)
          (list 'memq memq)
          (list 'member member)
          (list 'assoc assoc)
          (list 'not not)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list '= =)
          (list '< <)
          (list '> >)
          (list 'abs abs)
          (list 'remainder remainder)
          (list 'print print)
          ;; 基本手続きが続く
          ))
(define (primitive-procedure-names)
    (map car primitive-procedures))
(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
         primitive-procedures))

(define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme
        (primitive-implementation proc) args))

; Structure of the evaluator
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
    ((analyze exp) env succeed fail))

(define (analyze exp)
    (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
          ((quoted? exp) (analyze-quoted exp))
          ((variable? exp) (analyze-variable exp))
          ((assignment? exp) (analyze-assignment exp))
          ((definition? exp) (analyze-definition exp))
          ((if? exp) (analyze-if exp))
          ((let? exp) (analyze (let->combination exp)))
          ((lambda? exp) (analyze-lambda exp))
          ((begin? exp) (analyze-sequence (begin-actions exp)))
          ((cond? exp) (analyze (cond->if exp)))
          ((amb? exp) (analyze-amb exp)) ; Added amb
          ((application? exp) (analyze-application exp))
          (else (error "Unknown expression type -- ANALYZE" exp))))

; Simple expressions
(define (analyze-self-evaluating exp)
    (lambda (env succeed fail)
        (succeed exp fail)))

(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env succeed fail)
            (succeed qval fail))))

(define (analyze-variable exp)
    (lambda (env succeed fail)
        (succeed (lookup-variable-value exp env)
                 fail)))

(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyze-sequence (lambda-body exp))))
        (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env)
                     fail))))

; Conditionals and sequences
(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
        (lambda (env succeed fail)
            (pproc env
                   ;; pred-valueを得るための
                   ;; 述語の評価の成功継続
                   (lambda (pred-value fail2)
                    (if (true? pred-value)
                        (cproc env succeed fail2)
                        (aproc env succeed fail2)))
                   ;; 述語の評価の失敗継続
                   fail))))

(define (analyze-sequence exps)
    (define (sequentially a b)
        (lambda (env succeed fail)
            (a env
               ;; aを呼び出す時の成功継続
               (lambda (a-value fail2)
                (b env succeed fail2))
               ;; aを呼び出す時の失敗継続
               fail)))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE"))
        (loop (car procs) (cdr procs))))

; Definitions and assignments
(define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
        (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                       (define-variable! var val env)
                       (succeed 'ok fail2))
                   fail))))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
        (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)         ; *1*
                    (let ((old-value (lookup-variable-value var env)))
                        (set-variable-value! var val env)
                        (succeed 'ok
                                 (lambda ()    ; *2*
                                    (set-variable-value! var old-value env)
                        (fail2)))))
                   fail))))

; Procedure applications
(define (analyze-application exp)
    (let ((pproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
        (lambda (env succeed fail)
            (pproc env
                   (lambda (proc fail2)
                    (get-args aprocs
                              env
                              (lambda (args fail3)
                                (execute-application proc args succeed fail3))
                              fail2))
                   fail))))

(define (get-args aprocs env succeed fail)
    (if (null? aprocs)
        (succeed '() fail)
        ((car aprocs) env
                      ;; このaprocの成功継続
                      (lambda (arg fail2)
                        (get-args (cdr aprocs)
                                  env
                                  ;; get-argsの再帰呼出しの
                                  ;; 成功継続
                                  (lambda (args fail3)
                                    (succeed (cons arg args)
                                             fail3))
                                  fail2))
                      fail)))

(define (execute-application proc args succeed fail)
    (cond ((primitive-procedure? proc)
            (succeed (apply-primitive-procedure proc args)
                     fail))
          ((compound-procedure? proc)
            ((procedure-body proc)
                (extend-environment (procedure-parameters proc)
                                    args
                                    (procedure-environment proc))
                succeed
                fail))
          (else
            (error
                "Unknown procedure type -- EXECUTE-APPLICATION"
                proc))))

; Evaluating amb expressions
(define (analyze-amb exp)
    (let ((cprocs (map analyze (amb-choices exp))))
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda () (try-next (cdr choices))))))
            (try-next cprocs))))

; Driver loop
(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

(define (announce-output string)
    (newline) (display string) (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>))
        (display object)))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
    (define (internal-loop try-again)
        (prompt-for-input input-prompt)
        (let ((input (read)))
            (if (eq? input 'try-again)
                (try-again)
                (begin
                    (newline)
                    (display ";;; Starting a new problem ")
                    (ambeval input
                             the-global-environment
                             ;; ambeval 成功
                             (lambda (val next-alternative)
                                (announce-output output-prompt)
                                (user-print val)
                                (internal-loop next-alternative))
                             ;; ambeval 失敗
                             (lambda ()
                                (announce-output ";;; There are no more values of")
                                (user-print input)
                                (driver-loop)))))))
    (internal-loop
        (lambda ()
            (newline)
            (display ";;; There is no current problem")
            (driver-loop))))

(define the-global-environment (setup-environment))

(driver-loop)
; 上の行をコメントインして`rlwrap gosh ch4/ch4_3.scm` で実行
