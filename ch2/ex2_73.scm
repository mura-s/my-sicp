(use compat.sicp)

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

; a.
; 代数演算の記号をderiv演算を持つインタフェースを実装する型として、データ主導プログラミングの形に書き直している。
; 問題文中のderivでは、else句の中で、代数演算の記号のderivをgetし、その手続きを実行するようにしている。
;
; number?やvariable?は、引数が代数演算の記号を持たないため、吸収できない。

; b.
(define (install-sum-package)
    (define (addend s) (car s))
    (define (augend s) (cadr s))
    (define (make-sum a1 a2)
        (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))

    (put 'make '+ make-sum)
    (put 'deriv '+ deriv-sum)
    'done)

(define (install-product-package)
    (define (=number? exp num)
        (and (number? exp) (= exp num)))
    (define (multiplier p) (car p))
    (define (multiplicand p) (cadr p))
    (define (make-product m1 m2)
        (cond ((or (=number? m1 0) (=number? m2 0)) 0)
              ((=number? m1 1) m2)
              ((=number? m2 1) m1)
              ((and (number? m1) (number? m2)) (* m1 m2))
              (else (list '* m1 m2))))
    (define (deriv-product exp var)
        ((make '+
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp)))))

    (put 'make '* make-product)
    (put 'deriv '* deriv-product)
    'done)

(define (make op x y)
    ((get 'make op) x y))

; c.
(define (make-exponentiation-package)
    (define (base e) (car e))
    (define (exponent e) (cadr e))
    (define (make-exponentiation b e)
        (cond ((=number? e 0) 1)
              ((=number? e 1) b)
              (else (list '** b e))))
    (define (deriv-exponentiation exp var)
        (make '*
              (make '*
                    (exponent exp)
                    (make-exponentiation (base exp)
                                         (make '+ (exponent exp) -1)))
              (deriv (base exp) var)))

    (put 'make '** make-exponentiation)
    (put 'deriv '** deriv-exponentiation)
    'done)

; d.
; 各putの部分で<op>と<type>の部分を反対にすれば良い。
