; 特に同じ区分の中で, 入力が0, 1から1, 0に変ったアンドゲートの振舞いをトレースし, 
; 区分内の手続きを先頭で挿入, 削除する(最後に入ったものが最初に出る)通常のリストに格納した時との振舞いの違いを述べよ:
;
;   LIFOの場合、以下の順番で操作が行われたときに、(set-signal! input-1 1) のアクションが最後に実行されるが、
;   これは input1=1, input2=1 の AND の結果を出力してしまうため、FIFOの場合と結果が異なる。
;
;   (set-signal! input-1 0)
;   (set-signal! input-2 1)
;   (propagate)
;
;   (set-signal! input-1 1)
;   (set-signal! input-2 0)
;   (propagate)
