; a と b のコネクタを定義すると、手続き make-connector が実行されて、それぞれのコネクタの環境 E1 と E2 が作られる。
; コネクタ a に値を設定すると、環境 E1 に value と informant の値が設定されて、手続き for-each-except が評価される。
; この時 constraints は '() なので何も実行せずに done を返す。
