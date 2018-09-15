; 元のoutranked-by
(rule (outranked-by ?staff-person ?boss)
    (or (supervisor ?staff-person ?boss)
        (and (supervisor ?staff-person ?middle-manager)
             (outranked-by ?middle-manager ?boss))))

; 変更されたoutranked-by
(rule (outranked-by ?staff-person ?boss)
    (or (supervisor ?staff-person ?boss)
        (and (outranked-by ?middle-manager ?boss)
             (supervisor ?staff-person ?middle-manager))))

; 答を返した後, システムは無限ループに入った. なぜか説明せよ.
答えを返すのは、?staff-personが(Bitdiddle Ben)に束縛され、(supervisor ?staff-person ?boss)でbossが見つかるから。
無限ループに入るのは、(outranked-by ?middle-manager ?boss)が?middle-manager, ?bossとも未束縛のまま呼び出され続けるから。
