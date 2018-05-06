(use compat.sicp)

; 3.3.3
(define (lookup key table)
    (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))

(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

(define (insert! key value table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
    'ok)

(define (make-table)
    (list '*table*))

; (define tbl (make-table))
; (insert! 'a 1 tbl)
; (insert! 'b 2 tbl)
; (print tbl)

; Two-dimensional tables
(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    false))
            false)))

(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                              (cons (cons key-2 value)
                                    (cdr subtable)))))
            (set-cdr! table
                      (cons (list key-1 (cons key-2 value))
                            (cdr table)))))
    'ok)

; (define tbl (make-table))
; (insert! 'hoge 'a 1 tbl)
; (insert! 'hoge 'b 2 tbl)
; (insert! 'foo 'c 3 tbl)
; (print tbl)

; Creating local tables
(define (make-table)
    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                      (cons (cons key-2 value)
                                            (cdr subtable)))))
                    (set-cdr! local-table
                            (cons (list key-1
                                        (cons key-2 value))
                                  (cdr local-table)))))
            'ok)    
        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  (else (error "Unknown operation -- TABLE" m))))
        dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; (put 'hoge 'a 1)
; (put 'hoge 'b 2)
; (put 'foo 'c 3)
; (print (get 'hoge 'b))
; (print (get 'unknown 'a))
