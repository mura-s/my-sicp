(use compat.sicp)

(define (project x)
    (let ((proc (get 'project (type-tag x))))
        (if proc
            (proc (contents x))
            false)))

(define (install-rational-package)
    ; ...
    (define (project-to-scheme-number x)
        (make-scheme-number (round (/ (numer x) (denom x)))))
    (put 'project '(rational) project-to-scheme-number)
    ; ...
    'done)

(define (install-real-number-package)
    ; ...
    (define (project-to-rational x)
        (rationalize x)) ; rationalizeは省略
    (put 'project '(real-number) project-to-rational)
    ; ...
   'done)

(define (install-complex-package)
    ; ...
    (define (project-to-real-number x) (make-real-number (real-number x)))
    (put 'project '(complex) project-to-real-number)
    ; ...
    'done)

(define (drop x)
    (let ((projected-x (project x)))
        (if projected-x
            (if (equ? x (raise projected-x))
                (drop projected-x)
                x)
            x)))

; 最後に答を「単純化する」ように問題2.84のapply-genericを書き直すのにdropを使え:
;   (apply proc (map contents args)) を、 (drop (apply proc (map contents args))) に、
;   (apply proc (map contents raised-args)) を、 (drop (apply proc (map contents raised-args))) に書き直す.
