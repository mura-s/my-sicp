(use compat.sicp)

(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
    (tagged-list? exp 'set!))

(define (definition? exp)
    (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (begin? exp) (tagged-list? exp 'begin))

(define (cond? exp) (tagged-list? exp 'cond))

(define (application? exp) (pair? exp))

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
