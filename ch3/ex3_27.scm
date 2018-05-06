(use compat.sicp)

(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
    'ok)

(define (make-table)
    (list '*table*))

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

(define (memoize f)
    (let ((table (make-table)))
        (lambda (x)
            (let ((previously-computed-result (lookup x table)))
                (or previously-computed-result
                    (let ((result (f x)))
                        (insert! x result table)
                        result))))))

(define memo-fib
    (memoize (lambda (n)
               (cond ((= n 0) 0)
                     ((= n 1) 1)
                     (else (+ (memo-fib (- n 1))
                              (memo-fib (- n 2))))))))

(print (memo-fib 5))

; memo-fibがn番目のFibonacci数をnに比例したステップ数で計算出来る理由:
;   memoize内のtableに計算結果をメモ化しているため、 n-1 ~ 0 まで一度だけ計算すればよいため。

; memo-fibを単に(memoize fib)と定義してもやはり働くだろうか:
;   働かない。fibを再帰的に呼び出してしまい、計算結果をメモ化できないため。
