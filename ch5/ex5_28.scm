; ev-sequence
;   (test (op no-more-exps?) (reg unev))
;   (branch (label ev-sequence-end))
;   (assign exp (op first-exp) (reg unev))
;   (save unev)
;   (save env)
;   (assign continue (label ev-sequence-continue))
;   (goto (label eval-dispatch))
; ev-sequence-continue
;   (restore env)
;   (restore unev)
;   (assign unev (op rest-exps) (reg unev))
;   (goto (label ev-sequence))
; ev-sequence-end
;   (restore continue)
;   (goto (reg continue))
; を使用するように修正して、5.26と5.27を実行する
