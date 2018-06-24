(use compat.sicp)

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((while? exp) (eval (while->let exp) env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

(define (while? exp) (tagged-list? exp 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (while->let exp)
    (let ((condition (while-condition exp))
          (body (while-body exp)))
        (list 'let
              'while-loop
              '()
              (make-if condition
                       (make-begin (list body (cons 'while-loop '())))
                       'true))))
