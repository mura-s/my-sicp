; serialized-exchangeを呼ぶと、serialized-exchangeの中でlockを取った状態で、
; さらにwithdrawでlockを取ろうとするため、dead lockになってしまう。
