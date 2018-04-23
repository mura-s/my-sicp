(use compat.sicp)

; a.
(define (attach-tag type-tag content) (cons type-tag content))
(define (type-tag datum) (car datum))
(define (content datum) (cdr datum))

(define (get-record name file)
    ((get 'get-record (type-tag file))
        name
        (content file)))

(define (install-division-foo-package)
    (define (get-record name file)
        (...)) ; 実装は省略
    
    (define (tag x) (attach-tag 'division-foo x))
    (put 'get-record 'division-foo
         (lambda (name file) (tag (get-record name file))))
    'done)

; 型情報としては、事業所名を利用する。(事業所名のtagを事業所ファイルに付与する。)
; get-recordは、installされた各事業所ごとのget-recordをdispatchするように実装する。

; b.
(define (get-salary record)
    ((get 'get-salary (type-tag record)) (content record)))

; 従業員のレコードに事業所名のtagを付与しておく必要がある。

; c.
(define (find-employee-record name file-list)
    (if (null? file-list)
        nil
        (let ((record (get-record name (car file-list))))
            (if (content record)
                record
                (find-employee-record name (cdr file-list))))))

; d.
; 合併した別会社用のパッケージを実装してinstallする。
