(use compat.sicp)

(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ; 略
          ((quoted? exp) (text-of-quotation exp env))
          ; 略
          (else
            (error "Unknown expression type -- EVAL" exp))))

(define (text-of-quotation exp env)
    (if (list? (cadr exp))
        (eval (make-quotation-list (cadr exp)) env)
        (cadr exp)))

(define (make-quotation-list l)
    (if (null? l)
        '()
        (let ((first-list (car l))
              (rest-list (cdr l)))
            (list 'cons (list 'quote first-list) (make-quotation-list rest-list)))))
