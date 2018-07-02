(use compat.sicp)

(define (lookup-variable-value var env)
    (define (env-loop env)
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
                (scan var
                      frame
                      (lambda () (env-loop (enclosing-environment env)))
                      (lambda (vals) (car vals))))))
    (env-loop env))

(define (set-variable-value! var val env)
    (define (env-loop env)
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
                (scan var
                      frame
                      (lambda () (env-loop (enclosing-environment env)))
                      (lambda (vals) (set-car! vals val))))))
    (env-loop env))

(define (define-variable! var val env)
    (let ((frame (first-frame env)))
        (scan var
              frame
              (lambda () (add-binding-to-frame! var val frame))
              (lambda (vals) (set-car! vals val)))))

(define (scan var frame null-callback eq-callback)
    (define (scan-iter vars vals)
        (cond ((null? vars) (null-callback))
              ((eq? var (car vars)) (eq-callback vals))
              (else (scan-iter (cdr vars) (cdr vals)))))
    (scan-iter (frame-variables frame) (frame-values frame)))
