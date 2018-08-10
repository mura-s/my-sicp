(use compat.sicp)

; 導出された式としての実装
(define (analyze exp)
    (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
          ; 略
          ((unless? exp) (analyze (unless-if exp)))
          ; 略
          (else (error "Unknown expression type -- ANALYZE" exp))))

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        false))

(define (unless-if exp)
    (make-if (unless-predicate exp)
             (unless-alternative exp)
             (unless-consequent exp)))

; 手続きとして使えると有用である状況:
;   手続きとして使えるとmapなどに渡して使用することができる。
