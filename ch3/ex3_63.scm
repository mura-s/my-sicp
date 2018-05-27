(define (sqrt-stream x)
    (cons-stream 1.0
                (stream-map (lambda (guess)
                                (sqrt-improve guess x))
                            (sqrt-stream x))))

; Alyssaの答を説明せよ:
;   局所変数guessesを使わない場合、sqrt-streamの呼び出しごとにストリームが生成されてしまい、
;   計算が必要なので非効率。

; メモ化を使わない場合:
;   局所変数guessesを使った場合でも、各iterationごとにすべての計算が必要になるため、
;   局所変数guessesを使わない場合と同じように非効率になってしまう。
