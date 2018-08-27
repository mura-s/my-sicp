(use compat.sicp)
(use gauche.sequence)

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
          ((ramb? exp) (analyze-ramb exp)) ; Added ramb
          ((application? exp) (analyze-application exp))
          (else (error "Unknown expression type -- ANALYZE" exp))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
    (let ((cprocs (map analyze (shuffle (ramb-choices exp)))))
        (lambda (env succeed fail)
            (define (try-next choices)
                (if (null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda () (try-next (cdr choices))))))
            (try-next cprocs))))