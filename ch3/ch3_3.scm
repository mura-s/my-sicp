(use compat.sicp)

; 3.3.1
(define x (list (cons 'a 'b) 'c 'd))
(define y (list 'e 'f))

(print x)
(print y)

(set-car! x y)
(print x)

(set-cdr! x y)
(print x)

(define (get-new-pair) (cons nil nil))

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

(print (cons 'a 'b))
(newline)

; Sharing and identity
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
    (set-car! (car x) 'wow)
    x)

(print (set-to-wow! z1))
(print (set-to-wow! z2))
(newline)

; Mutation is just assignment
(define (cons x y)
    (define (set-x! v) (set! x v))
    (define (set-y! v) (set! y v))
    (define (dispatch m)
        (cond ((eq? m 'car) x)
              ((eq? m 'cdr) y)
              ((eq? m 'set-car!) set-x!)
              ((eq? m 'set-cdr!) set-y!)
              (else (error "Undefined operation -- CONS" m))))
    dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
    ((z 'set-car!) new-value)
    z)
(define (set-cdr! z new-value)
    ((z 'set-cdr!) new-value)
    z)

; 3.3.2
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
                (set-front-ptr! queue new-pair)
                (set-rear-ptr! queue new-pair)
                queue)
              (else
                (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
            (error "DELETE! called with an empty queue" queue))
          (else
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))
