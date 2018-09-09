(rule (replace person1 person2)
    (and (job ?person1 ?job1)
         (job ?person2 ?job2)
         (not (same ?person1 ?person2))
         (or (same ?job1 ?job2))
             (can-do-job ?job1 ?job2)))

; a.
(replace ?person (Fect Cy D))

; b.
(and (replace ?x ?y)
     (lisp-value < ?x ?y)
     (salary ?x ?amount1)
     (salary ?y ?amount2))
