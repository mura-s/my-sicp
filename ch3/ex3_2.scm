(use compat.sicp)

(define (make-monitored f)
    (let ((counter 0))
        (define (execute-func arg)
            (set! counter (+ counter 1))
            (f arg))
        (define (how-many-calls?) counter)
        (define (reset) (set! counter 0))
        (define (mf arg)
            (cond ((eq? arg 'how-many-calls?) (how-many-calls?))
                  ((eq? arg 'reset) (reset))
                  (else (execute-func arg))))
        mf))

(define s (make-monitored sqrt))

(print (s 100))
(print (s 9))
(print (s 'how-many-calls?))
(print (s 'reset))
(print (s 'how-many-calls?))
