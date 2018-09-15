(rule (wheel ?person)
    (and (supervisor ?middle-manager ?person)
         (supervisor ?x ?middle-manager)))

; Warbucks Oliverはなぜ四回も出力されたか
(supervisor ?middle-manager ?person)が?middle-manager=Ben, ?person=Oliverに束縛されるケースでは、
Benは3人のsupervisorになっているため、(supervisor ?x ?middle-manager)で3回束縛されるので、Oliverが3回出力される。
また、OliverはEbenのsupervisorでもあり、Ebenが1人のsupervisorであるため、もう1回出力される。
