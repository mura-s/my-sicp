(use compat.sicp)

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))

(define (leaf? object)
    (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((eq? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (if (leaf? tree)
        '()
        (let ((lb (left-branch tree))
              (rb (right-branch tree)))
             (cond ((element-of-set? symbol (symbols lb))
                    (cons 0 (encode-symbol symbol lb)))
                   ((element-of-set? symbol (symbols rb))
                    (cons 1 (encode-symbol symbol rb)))
                   (else (error "error"))))))

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))

(print (encode '(A D A B B C A) sample-tree))
