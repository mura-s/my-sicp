(use compat.sicp)

(define (install-complex-package)
    ; ...
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    ; ...
    'done')

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error
                    "No method for these types -- APPLY-GENERIC"
                    (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; 手続きのトレース:
;   (magnitude z)
;   (apply-generic 'magnitude z)
;   (magnitude (contents z)) ; complex-packageのmagnitudeを適用. 以降 rec = (contents z) とする.
;   (apply-generic 'magnitude rec)
;   (magnitude (contents rec)) ; rectangular-packageのmagnitudeを適用.

; apply-genericは2回呼び出される.
