; Benの主張が正しい。
; 元のコードではdiffを取って交換する間に別のプロセスが走ると問題になっていたが、
; このコードではその処理がない。
; また、withdrawとdepositの間に他のプロセスが動いたとしても、depositではロックを取って
; 引数のamountをbalanceに追加するだけなので問題ない。