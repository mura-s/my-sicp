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

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE_BRANCH" bit))))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
                        (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
        (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (if (= (length leaf-set) 1)
        (car leaf-set)
        (successive-merge
            (adjoin-set (make-code-tree (car leaf-set)
                                        (cadr leaf-set))
                        (cddr leaf-set)))))

(define pairs1 (list (list 'A 4)
                     (list 'B 2)
                     (list 'C 1)
                     (list 'D 1)))

(print (generate-huffman-tree pairs1))

(define pairs2 (list (list 'A 8)
                     (list 'B 3)
                     (list 'C 1)
                     (list 'D 1)
                     (list 'E 1)
                     (list 'F 1)
                     (list 'G 1)
                     (list 'H 1)))

(print (generate-huffman-tree pairs2))