(use compat.sicp)

(define f
    (let ((value 0)
          (set false))
    (lambda (n)
        (if set
            value
            (begin (set! value n)
                   (set! set true)
                   0)))))

(print (+ (f 0) (f 1)))
