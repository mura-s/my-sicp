; あるプロセスが (set-car! cell true) を実行する前に、
; 別のプロセスが (car cell) のチェックを行なってしまうと、こちらのプロセスもロックを獲得できてしまう。
