; letを使い内部定義を掃き出した場合、letは更にlambda式に書き換えられる。
; lambda式はフレームを拡張するため、このときにフレームが生成される。

; プログラムの行動に違いが出ないのは、フレームを追加するだけで変数の定義も変更もしないため。

; フレームを生じさせないためには、letを使うのではなく、各defineをbodyのtopに持ってくるように
; 変換すればよい。

; ref. http://d.hatena.ne.jp/tmurata/20100324/1269432404
