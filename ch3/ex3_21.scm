(use compat.sicp)

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

(define q1 (make-queue))
(print (insert-queue! q1 'a))
(print (insert-queue! q1 'b))
(print (delete-queue! q1))
(print (delete-queue! q1))
(newline)

; Eva Luのいっていることを説明せよ. 特にBenの例がどうしてあのような印字結果を生じたか説明せよ:
;   このキューの実装では、make-queueが返すqueueの指すconsが、car=実際のキューの先頭へのポインタ、
;   cdr=実際のキューの末尾へのポインタ、という構造になっているため。
;   また、delete-queue!の結果については、delete-queue!ではfront-ptrの指す先を移動するだけなので、
;   rear-ptrの値は変わらないため。

(define (print-queue q)
    (define (list-queue q)
        (if (null? q)
            nil
            (cons (car q) (list-queue (cdr q)))))
    (print (list-queue (front-ptr q))))

(define q2 (make-queue))
(insert-queue! q2 'a)
(insert-queue! q2 'b)
(insert-queue! q2 'c)
(delete-queue! q2)
(print-queue q2)
