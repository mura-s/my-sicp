(use compat.sicp)

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((unbind? exp) (eval-unbinding exp env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

(define (eval-unbinding exp env)
    (unbind-variable! (unbinding-variable exp) env)
    'ok)

(define (unbind? exp) (tagged-list? exp 'unbind!))
(define (unbinding-variable exp) (cadr exp))

(define (unbind-variable! var env)
    (let (frame (first-fram env)))
        (define (scan vars vals)
            (define (unbind)
                (if (null? (cdr vars))
                    (begin
                        (set-car! vars '())
                        (set-car! vals '()))
                    (begin
                        (set-car! vars (cadr vars))
                        (set-cdr! vars (cddr vars))
                        (set-car! vals (cadr vals))
                        (set-cdr! vals (cddr vals)))))
            (cond ((null? vars) (error "Unbind variable" var))
                  ((eq? var (car vars)) (unbind))
                  (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame)))
