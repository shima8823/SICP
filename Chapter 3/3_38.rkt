#|

total=100
a=Peter : ( set! balance (+ balance 10))
b=Paul  : ( set! balance (- balance 20))
c=Mary  : ( set! balance (- balance (/ balance 2)))

a.
abc = 45
acb = 35
bac = 45
bca = 50
cab = 40
cba = 40

b.
a	= 110
b	= 80
c	= 50
ab	= 90
ac	= 55
ba	= 90
bc	= 40
ca	= 60
cb	= 20

※他にもいっぱいある
例えばMaryは2つbalanceをとっているので2つの間にwriteされる可能性がある。

|#