(use compat.sicp)

; assignmentを (set! var value) ではなく (var := value) で行うようにする。

(define (assignment? exp)
    (if (and (pair? exp) (tagged-list? (cdr exp) ':=))
        true
        false))

(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))
