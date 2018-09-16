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

(define (stream-filter pred stream)
    (cond ((stream-null? stream)
            the-empty-stream)
          ((pred (stream-car stream))
            (cons-stream (stream-car stream)
                         (stream-filter pred (stream-cdr stream))))
          (else
             (stream-filter pred (stream-cdr stream)))))

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

; 4.4.4.2
(define (qeval query frame-stream)
    (let ((qproc (get (type query) 'qeval)))
        (if qproc
            (qproc (contents query) frame-stream)
            (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
    (stream-flatmap
        (lambda (frame)
            (stream-append-delayed
                (find-assertions query-pattern frame)
                (delay (apply-rules query-pattern frame))))
        frame-stream))

(define (conjoin conjuncts frame-stream)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (conjoin (rest-conjuncts conjuncts)
                 (qeval (first-conjunct conjuncts)
                        frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
    (if (empty-disjunction? disjuncts)
        the-empty-stream
        (interleave-delayed
            (qeval (first-disjunct disjuncts) frame-stream)
            (delay (disjoin (rest-disjuncts disjuncts)
                            frame-stream)))))
(put 'or 'qeval disjoin)

(define (negate operands frame-stream)
    (stream-flatmap
        (lambda (frame)
            (if (stream-null? (qeval (negated-query operands)
                                     (singleton-stream frame)))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
    (stream-flatmap
        (lambda (frame)
            (if (execute
                    (instantiate
                        call
                        frame
                        (lambda (v f)
                            (error "Unknown pat var -- LISP-VALUE" v))))
                (singleton-stream frame)
                the-empty-stream))
        frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
    (apply (eval (predicate exp) user-initial-environment)
           (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

; 4.4.4.3
(define (find-assertions pattern frame)
    (stream-flatmap (lambda (datum)
                        (check-an-assertion datum pattern frame))
                    (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
    (let ((match-result
            (pattern-match query-pat assertion query-frame)))
        (if (eq? match-result 'failed)
            the-empty-stream
            (singleton-stream match-result))))

(define (pattern-match pat dat frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? pat dat) frame)
          ((var? pat) (extend-if-consistent pat dat frame))
          ((and (pair? pat) (pair? dat))
            (pattern-match (cdr pat)
                           (cdr dat)
                           (pattern-match (car pat)
                                          (car dat)
                                          frame)))
          (else 'failed)))

(define (extend-if-consistent var dat frame)
    (let ((binding (binding-in-frame var frame)))
        (if binding
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame))))

; 4.4.4.4
(define (apply-rules pattern frame)
    (stream-flatmap (lambda (rule)
                        (apply-a-rule rule pattern frame))
                    (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
    (let ((clean-rule (rename-variables-in rule)))
        (let ((unify-result
            (unify-match query-pattern
                         (conclusion clean-rule)
                         query-frame)))
        (if (eq? unify-result 'failed)
            the-empty-stream
            (qeval (rule-body clean-rule)
                   (singleton-stream unify-result))))))

(define (rename-variables-in rule)
    (let ((rule-application-id (new-rule-application-id)))
        (define (tree-walk exp)
            (cond ((var? exp)
                    (make-new-variable exp rule-application-id))
                  ((pair? exp)
                    (cons (tree-walk (car exp))
                          (tree-walk (cdr exp))))
                  (else exp)))
        (tree-walk rule)))

(define (unify-match p1 p2 frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))  ; ***
          ((and (pair? p1) (pair? p2))
            (unify-match (cdr p1)
                         (cdr p2)
                         (unify-match (car p1)
                                      (car p2)
                                      frame)))
          (else 'failed)))

(define (extend-if-possible var val frame)
    (let ((binding (binding-in-frame var frame)))
        (cond (binding
                (unify-match
                    (binding-value binding) val frame))
              ((var? val)                      ; ***
                (let ((binding (binding-in-frame val frame)))
                    (if binding
                        (unify-match
                            var (binding-value binding) frame)
                        (extend var val frame))))
              ((depends-on? val var frame)     ; ***
                'failed)
              (else (extend var val frame)))))

(define (depends-on? exp var frame)
    (define (tree-walk e)
        (cond ((var? e)
                (if (equal? var e)
                    true
                    (let ((b (binding-in-frame e frame)))
                        (if b
                            (tree-walk (binding-value b))
                            false))))
              ((pair? e)
                (or (tree-walk (car e))
                    (tree-walk (cdr e))))
              (else false)))
    (tree-walk exp))
