; a.
(meeting ?x (Friday ?y))

; b.
(rule (meeting-time ?person ?day-and-time)
    (or (meeting whole-company ?day-and-time)
        (and (job ?person (?div . ?type))
             (meeting ?div ?day-and-time))))

; c.
(and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
     (meeting ?div (Wednesday ?time)))
