(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

(define (parse-word word-list)
    (require (not (null? *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list)
          (an-element-of (cdr word-list))))
