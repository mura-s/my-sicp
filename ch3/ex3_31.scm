; 特に上の段落での半加算器の例をトレースし, accept-action-procedure!を
;   (define (accept-action-procedure! proc)
;   (set! action-procedures (cons proc action-procedures)))
; のように定義したら, システムの応答はどう違うか:
;
;   新しいアクション手続きを回線に追加したときに初期化が行われないため、
;   例えば、inverterの出力の値も0のままになってしまう。
;   その状態で、半加算器のAの値を1にしたとしても、ANDを取ってSは0を出力してしまう。
