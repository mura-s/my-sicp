start
    (goto (label here))
here
    (assign a (const 3))
    (goto (label there))
here
    (assign a (const 4))
    (goto (label there))
there

; 制御がthereに達した時レジスタa の内容はどうなるか
lookup-labelで1つ目のhereが見つかるため、aは3になる。

; extract-labels手続きを修正し, 同じラベル名が二つの異る場所を指すように使われたら, エラーとするようにせよ
(define (extract-labels text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels
            (cdr text)
            (lambda (insts labels)
                (let ((next-inst (car text)))
                    (if (symbol? next-inst)
                        (if (assoc next-inst labels)
                            (error "Multiply defined label: ", next-inst)
                            (receive insts
                                    (cons (make-label-entry next-inst insts)
                                          labels)))
                        (receive (cons (make-instruction next-inst)
                                       insts)
                                 labels)))))))
