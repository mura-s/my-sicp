(rule (grandson-of ?g ?s)
    (and (son-of ?f ?s)
         (son-of ?g ?f)))

(rule (son-of ?m ?s)
    (or (son ?m ?s)
        (and (wife ?m ?w)
             (son ?w ?s))))
