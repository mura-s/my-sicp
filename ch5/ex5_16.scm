; trace関連の処理を追加

(use compat.sicp)

(define (make-new-machine)
    (let ((pc (make-register 'pc))
          (flag (make-register 'flag))
          (stack (make-stack))
          (trace-flag false)
          (instruction-count 0)
          (the-instruction-sequence '()))
        (let ((the-ops
                (list (list 'initialize-stack
                            (lambda () (stack 'initialize)))
                      (list 'print-stack-statistics
                            (lambda () (stack 'print-statistics)))))
              (register-table
                (list (list 'pc pc) (list 'flag flag))))
            (define (allocate-register name)
                (if (assoc name register-table)
                    (error "Multiply defined register: " name)
                    (set! register-table
                        (cons (list name (make-register name))
                              register-table)))
                'register-allocated)
            (define (lookup-register name)
                (let ((val (assoc name register-table)))
                    (if val
                        (cadr val)
                        (error "Unknown register:" name))))
            (define (execute)
                (let ((insts (get-contents pc)))
                    (if (null? insts)
                        'done
                        (begin
                            (if trace-flag
                                (print (caar insts)))
                            ((instruction-execution-proc (car insts)))
                            (set! instruction-count (+ instruction-count 1))
                            (execute)))))
            (define (print-and-init-instruction-count)
                (display (list "instruction count:" instruction-count))
                (newline)
                (set! instruction-count 0))
            (define (trace-on) (set! trace-flag true))
            (define (trace-off) (set! trace-flag false))
            (define (dispatch message)
                (cond ((eq? message 'start)
                        (set-contents! pc the-instruction-sequence)
                        (execute))
                      ((eq? message 'install-instruction-sequence)
                        (lambda (seq) (set! the-instruction-sequence seq)))
                      ((eq? message 'allocate-register) allocate-register)
                      ((eq? message 'get-register) lookup-register)
                      ((eq? message 'install-operations)
                        (lambda (ops) (set! the-ops (append the-ops ops))))
                      ((eq? message 'stack) stack)
                      ((eq? message 'operations) the-ops)
                      ((eq? message 'print-and-init-instruction-count)
                        (print-and-init-instruction-count))
                      ((eq? message 'trace-on) (trace-on))
                      ((eq? message 'trace-off) (trace-off))
                      (else (error "Unknown request -- MACHINE" message))))
            dispatch)))
