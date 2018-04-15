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
                   (else (error "error" symbol (symbols lb) (symbols rb)))))))

(define huffman-tree
    (generate-huffman-tree (list (list 'a 2)
                                 (list 'na 16)
                                 (list 'boom 1)
                                 (list 'Sha 3)
                                 (list 'Get 2)
                                 (list 'yip 9)
                                 (list 'job 2)
                                 (list 'Wah 1))))
(define lyric
    '(Get a job
      Sha na na na na na na na na
      Get a job
      Sha na na na na na na na na
      Wah yip yip yip yip yip yip yip yip yip
      Sha boom))

(define encoded (encode lyric huffman-tree))
(print encoded)
(print (length encoded))
; 符号化に84bit必要

(print (* 3 (length lyric)))
; 八記号アルファベットの固定長符号を使うと、108bit必要
