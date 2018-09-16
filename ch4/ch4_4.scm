(use compat.sicp)

; 4.4.1
; (rule (lives-near ?person-1 ?person-2)
;     (and (address ?person-1 (?town . ?rest-1))
;          (address ?person-2 (?town . ?rest-2))
;          (not (same ?person-1 ?person-2))))

; (rule (same ?x ?x))

; (rule (wheel ?person)
;     (and (supervisor ?middle-manager ?person)
;          (supervisor ?x ?middle-manager)))

; (rule (outranked-by ?staff-person ?boss)
;     (or (supervisor ?staff-person ?boss)
;         (and (supervisor ?staff-person ?middle-manager)
;              (outranked-by ?middle-manager ?boss))))

; (rule (append-to-form () ?y ?y))
; (rule (append-to-form (?u . ?v) ?y (?u . ?z))
;     (append-to-form ?v ?y ?z))

; 4.4.2
; (assert! (job (Bitdiddle Ben) (computer wizard)))

; (assert! (rule (wheel ?person)
;             (and (supervisor ?middle-manager ?person)
;                  (supervisor ?x ?middle-manager))))

; 4.4.3

; 4.4.4
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-line x)
    (newline)
    (display x))

(define (display-stream s)
    (stream-for-each display-line s))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

; 4.4.4.1
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
    (prompt-for-input input-prompt)
    (let ((q (query-syntax-process (read))))
        (cond ((assertion-to-be-added? q)
                (add-rule-or-assertion! (add-assertion-body q))
                (newline)
                (display "Assertion added to data base.")
                (query-driver-loop))
              (else
                (newline)
                (display output-prompt)
                (display-stream
                    (stream-map
                        (lambda (frame)
                            (instantiate q
                                         frame
                                         (lambda (v f) (contract-question-mark v))))
                        (qeval q (singleton-stream '()))))
                (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
    (define (copy exp)
        (cond ((var? exp)
                (let ((binding (binding-in-frame exp frame)))
                    (if binding
                        (copy (binding-value binding))
                        (unbound-var-handler exp frame))))
              ((pair? exp)
                (cons (copy (car exp)) (copy (cdr exp))))
              (else exp)))
    (copy exp))
