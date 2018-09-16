(rule ((great . ?rel) ?x ?y)
    (and (son-of ?x ?w)
         (?rel ?w ?y)))

(rule ((grandson) ?x ?y)
    (grandson-of ?x ?y))
