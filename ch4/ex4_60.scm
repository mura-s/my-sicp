(define (person->string person) 
    (if (null? person) 
        "" 
        (string-append (symbol->string (car person)) (person->string (cdr person)))))

(define (person>? p1 p2) 
    (string>? (person->sring p1) (person->string p2)))

(rule (lives-near ?person-1 ?person-2)
    (and (address ?person-1 (?town . ?rest-1))
         (address ?person-2 (?town . ?rest-2))
         (lisp-value person>? ?person-1 ?person-2)))
