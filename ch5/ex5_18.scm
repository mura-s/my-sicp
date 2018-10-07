(define (make-register name)
    (let ((contents '*unassigned*)
          (trace-flag false))
        (define (dispatch message)
            (cond ((eq? message 'get) contents)
                  ((eq? message 'set)
                    (lambda (value)
                        (if trace-flag
                            (print "register name: " name ", old value: " contents ", new value: " value))
                        (set! contents value)))
                  ((eq? message 'trace-on) (set! trace-flag true))
                  ((eq? message 'trace-off) (set! trace-flag false))
                  (else
                    (error "Unknown request -- REGISTER" message))))
        dispatch))

(define (register-trace-on machine reg-name)
    ((get-register machine reg-name) 'trace-on))

(define (register-trace-off machine reg-name)
    ((get-register machine reg-name) 'trace-off))
