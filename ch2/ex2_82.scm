(use compat.sicp)

(define (apply-generic op . args)
    (define (coer now-t)
        (lambda (t)
            (if (eq? now-t t)
                (lambda (a) a)
                (get-coercion t now-t))))
    (define (apply-all-coercions args coercions)
        (if (null? args)
            nil
            (cons ((car coercions) (car args))
                  (apply-all-coercions (cdr args) (cdr coercions)))))
    (define (valid-coercions? coercions)
        (if (all-coercions-not-null? coercions)
            (let ((type-tags (map type-tag (apply-all-coercions args coercions))))
                (let ((proc (get op type-tags)))
                    (if proc true false)))
            false))
    (define (all-coercions-not-null? coercions)
        (cond ((null? coercions) true)
              ((null? (car coercions)) false)
              (else (all-coercions-not-null? (cdr coercions)))))
    (define (iter type-tags rest-args)
        (if (null? rest-args)
            (error "No method for these types" (list op type-tags))
            (let ((now-a (car rest-args))
                (let ((now-t (type-tag now-a))
                      (now-c (content now-a)))
                    (let ((coercions (map (coer now-t) type-tags)))
                        (if (valid-coercions? coercions)
                            (apply-generic op (apply-all-coercions args coercions))
                            (iter type-tags (cdr rest-args)))))))))
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (iter type-tags args))))) ; すべての引数を第一引数の型, 次に第二引数の型等々に強制変換を試みる

; この戦略が(そして上の二引数版が)十分に一般的でない状況の例:
;   全ての引数を全く異なる型 (例えば全ての引数のsuper-typeなど) に変換すれば適用可能な手続きがあるとしても、
;   そのような変換は試みられることはない.
