(use compat.sicp)

; 4.4.1
; (rule (lives-near ?person-1 ?person-2)
;     (and (address ?person-1 (?town . ?rest-1))
;          (address ?person-2 (?town . ?rest-2))
;          (not (same ?person-1 ?person-2))))

; (rule (same ?x ?x))

; (rule (wheel ?person)
;     (and (supervisor ?middle-manager ?person)
;          (supervisor ?x ?middle-manager)))

; (rule (outranked-by ?staff-person ?boss)
;     (or (supervisor ?staff-person ?boss)
;         (and (supervisor ?staff-person ?middle-manager)
;              (outranked-by ?middle-manager ?boss))))

; (rule (append-to-form () ?y ?y))
; (rule (append-to-form (?u . ?v) ?y (?u . ?z))
;     (append-to-form ?v ?y ?z))

; 4.4.2
; (assert! (job (Bitdiddle Ben) (computer wizard)))

; (assert! (rule (wheel ?person)
;             (and (supervisor ?middle-manager ?person)
;                  (supervisor ?x ?middle-manager))))

; 4.4.3
