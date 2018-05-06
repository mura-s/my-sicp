(use compat.sicp)

; item
(define (make-item value prev next)
    (list value prev next))

(define (value-of-item item) (car item))
(define (prev-item item) (cadr item))
(define (next-item item) (caddr item))

(define (set-prev-item! item prev)
    (set-car! (cdr item) prev))
(define (set-next-item! item next)
    (set-car! (cddr item) next))

; queue
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (value-of-item (front-ptr queue))))

(define (rear-queue queue)
    (if (empty-queue? queue)
        (error "REAR called with an empty queue" queue)
        (value-of-item (rear-ptr queue))))

(define (front-insert-queue! queue item)
    (let ((new-item (make-item item '() '())))
        (cond ((empty-queue? queue)
                (set-front-ptr! queue new-item)
                (set-rear-ptr! queue new-item)
                queue)
              (else
                (set-next-item! new-item (front-ptr queue))
                (set-prev-item! (front-ptr queue) new-item)
                (set-front-ptr! queue new-item)
                queue))))

(define (rear-insert-queue! queue item)
    (let ((new-item (make-item item '() '())))
        (cond ((empty-queue? queue)
                (set-front-ptr! queue new-item)
                (set-rear-ptr! queue new-item)
                queue)
              (else
                (set-prev-item! new-item (rear-ptr queue))
                (set-next-item! (rear-ptr queue) new-item)
                (set-rear-ptr! queue new-item)
                queue))))

(define (front-delete-queue! queue)
    (cond ((empty-queue? queue)
            (error "DELETE! called with an empty queue" queue))
          (else
            (set-front-ptr! queue (next-item (front-ptr queue)))
            queue)))

(define (rear-delete-queue! queue)
    (cond ((empty-queue? queue)
            (error "DELETE! called with an empty queue" queue))
          (else
            (set-rear-ptr! queue (prev-item (rear-ptr queue)))
            (if (null? (rear-ptr queue))
                (set-front-ptr! queue '()))
            queue)))

(define (print-queue-from-front q)
    (define (list-queue item)
        (if (null? item)
            nil
            (cons (value-of-item item)
                  (list-queue (next-item item)))))
    (print (list-queue (front-ptr q))))

(define (print-queue-from-rear q)
    (define (list-queue item)
        (if (null? item)
            nil
            (cons (value-of-item item)
                  (list-queue (prev-item item)))))
    (print (list-queue (rear-ptr q))))

; test
(define q1 (make-queue))
(front-insert-queue! q1 'a)
(rear-insert-queue! q1 'b)
(front-insert-queue! q1 'c)
(print-queue-from-front q1)
(print-queue-from-rear q1)
(print (front-queue q1))
(print (rear-queue q1))
(newline)

(define q2 (make-queue))
(front-insert-queue! q2 'a)
(front-insert-queue! q2 'a)
(front-delete-queue! q2)
(front-delete-queue! q2)
(front-insert-queue! q2 'a)
(print-queue-from-front q2)
(newline)

(define q3 (make-queue))
(front-insert-queue! q3 'a)
(rear-insert-queue! q3 'a)
(rear-delete-queue! q3)
(rear-delete-queue! q3)
(front-insert-queue! q3 'a)
(rear-delete-queue! q3)
(print-queue-from-front q3)
(newline)
