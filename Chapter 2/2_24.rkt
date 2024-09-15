#lang racket

#|

結果
(1 (2 (3 4)))

箱-点構造
(1 (2 (3 4)))->ab
1(a)
(2 (3 4)) (b)->cd
2(c)
(3 4) (d)->ef
3(e)
4(f)

木構造
x = (1 (2 (3 4)))
a = 1
b = (2 (3 4))
c = 2
d = (3 4)
e = 3
f = 4

			x
	a					b
				c				d
							e		f



|#

(list 1 (list 2 (list 3 4)))
