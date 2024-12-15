#|

p2=p1.p3
p3=p1.e0

x=(cons 1 2)=1.2 =p1

the-cars	n1	p1	p1
the-cdrs	n2	p3	e0

p1=x
p2.p3=y
free=p4

answer
p2はp3が作られてから出ないとp3と指定できないはずなので、逆になる。

p2=p1.e0
p3=p1.p2

the-cars	n1	p1	p1
the-cdrs	n2	e0	e2

|#