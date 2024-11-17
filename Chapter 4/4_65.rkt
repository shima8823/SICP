(rule (wheel ?person)
	(and (supervisor ?middle-manager ?person)
		 (supervisor ?x ?middle-manager)))

#|

;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

;;; Query results:
(Alyssa	Ben		Oliver)
(Louis	Alyssa	Ben)
(Lem	Ben		Oliver) 
(Robert	Eben	Oliver) → 経理部門
(Cy		Ben		Oliver) 

wheelは(x middle-manager wheel)の順で三人の地位が組み合わさったものを出力する。
よって上記のようにOliverと1個地位が下の人、さらにもう1個地位が下の人の組み合わせがOliverを4回出力されている。


|#