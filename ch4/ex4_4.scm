(use compat.sicp)

(define (eval-and exp env)
    (define (iter exp env last-val)
        (if (null? exp))
            last-val
            (let ((first-val (eval (and-first-exp exp) env))
                  (rest (and-rest-exp exp)))
                (if (true? first-val))
                    (iter rest env (first-val))
                    'false))
    (iter exp env 'true))

(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (and-first-exp exp) (car exp))
(define (and-rest-exp exp) (cdr exp))

(define (eval-or exp env)
    (if (null? exp)
        'false
        (let ((first-val (eval (or-first-exp exp) env))
              (rest (or-rest-exp exp)))
            (if (true? first-val)
                first-val
                (eval-or rest env)))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (or-first-exp exp) (car exp))
(define (or-rest-exp exp) (cdr exp))

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((and? exp) (eval-and (and-clauses exp) env))
          ((or? exp) (eval-or (or-clauses exp) env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

; 導出された式として評価する場合は、cond->ifのようにand->if, or->ifを用意する。
; 実装は略。
