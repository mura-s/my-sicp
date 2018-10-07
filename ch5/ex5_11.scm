; a.
以下の部分で、nレジスタに直接restoreすることができる。
afterfib-n-2
    (assign n (reg val))
    (restore val)

; b.
(define (make-save inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine reg-name)))
            (lambda ()
                (push stack (cons reg-name (get-contents reg)))
                (advance-pc pc)))))

(define (make-restore inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
        (let ((reg (get-register machine reg-name)))
            (lambda ()
                (let ((reg-name-and-content (pop stack)))
                    (if (eq? reg-name (car reg-name-and-content))
                        (set-contents! reg (pop stack))
                        (error "Wrong register name -- MAKE-RESTORE" reg-name))
                    (advance-pc pc))))))

; c.
; pass
