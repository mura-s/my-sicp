(use compat.sicp)

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-dense-term-list-package)
    (define (first-term-dense term-list)
        (make-term (- (length term-list) 1) (car term-list)))
    (define (rest-terms-dense term-list) (cdr term-list))
    (define (empty-termlist-dense? term-list) (null? term-list))
    (define (the-empty-termlist-dense) '())

    (define (adjoin-term-dense term term-list)
        (cond ((=zero? (coeff term)) term-list)
              ((equ? (order term) (length term-list))
                (cons (coeff term) term-list))
              (else
                (adjoin-term term (cons 0 term-list)))))

    (define (tag l) (attach-tag 'dense l))
    (put 'first-term 'dense first-term-dense)
    (put 'rest-terms 'dense (tag rest-terms-dense))
    (put 'empty-termlist? 'dense empty-termlist-dense?)
    (put 'the-empty-termlist 'dense (tag the-empty-termlist-dense))

    (put 'adjoin-term 'dense
        (lambda (term term-list) (tag (adjoin-term term term-list))))
    'done)

(define (install-sparse-term-list-package)
    (define (first-term-sparse term-list) (car term-list))
    (define (rest-terms-sparse term-list) (cdr term-list))
    (define (empty-termlist-sparse? term-list) (null? term-list))
    (define (the-empty-termlist-sparse) '())

    (define (adjoin-term-sparse term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))

    (define (tag l) (attach-tag 'sparse l))
    (put 'first-term 'sparse first-term-sparse)
    (put 'rest-terms 'sparse (tag rest-terms-sparse))
    (put 'empty-termlist? 'sparse empty-termlist-sparse?)
    (put 'the-empty-termlist 'sparse (tag the-empty-termlist-sparse))

    (put 'adjoin-term 'sparse
        (lambda (term term-list) (tag (adjoin-term term term-list))))
    'done)

(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
(define (the-empty-termlist type-tag) (apply-generic 'the-empty-termlist type-tag))

(define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list)) term term-list))
