(rule (reverse () ()))

(rule (reverse (?x . ?y) ?z)
    (and (reverse ?y ?v)
         (append-to-form ?v (?x) ?z)))

; 上記は(reverse (1 2 3) ?x)に対してのみ動作する。
; 
; reverseとappend-to-formを逆にすると、(reverse ?x (1 2 3))に対してのみ動作するようになる。
