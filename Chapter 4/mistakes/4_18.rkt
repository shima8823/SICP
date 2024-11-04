#lang sicp

#|

(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
	y)

練習問題
(lambda ⟨vars⟩
	(let ((u '*unassigned*) (v '*unassigned*))
		(let ((a ⟨e1⟩) (b ⟨e2⟩))
			(set! u a)
			(set! v b))
			⟨e3⟩))
うまく動く。
; answer
うまく動かないlet a bの時点でe1 e2を評価してしまう。
aは(delay dy)を使っているので実行できるが、
bは直接yを使用しているのでstream-mapは値を返さなくてはならない。
	そこでy=*unassignedでエラーになる。
*letであるならば、うまくいくだろう。


本文中
(lambda ⟨vars⟩
	(let ((u '*unassigned*) (v '*unassigned*))
		(set! u ⟨e1⟩)
		(set! v ⟨e2⟩)
		⟨e3⟩))
うまく動く。

|#
