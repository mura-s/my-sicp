; a. Ben Bitdiddleが監督している人すべての名前とその住所; 
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

; b. 給料がBen Bitdiddleのそれより少ない人のすべてと, その人たちの給料と, Ben Bitdiddleの給料; 
(and (salary (Bitdiddle Ben) ?amount1)
     (salary ?person ?amount2)
     (lisp-value < ?amount2 ?amount1))

; c. 計算機部門にいない人が監督している人すべてと, その監督者の名前と担当. 
(and (not (job ?person1 (computer . ?work)))
     (supervisor ?person2 ?person1)
     (job ?person1 ?job))
