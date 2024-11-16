(rule (outranked-by ?staff-person ?boss)
	(or (supervisor ?staff-person ?boss)
		(and (supervisor ?staff-person ?middle-manager)
			 (outranked-by ?middle-manager ?boss))))

; Louis
(rule (outranked-by ?staff-person ?boss)
	(or (supervisor ?staff-person ?boss)
		(and (outranked-by ?middle-manager ?boss)
			 (supervisor ?staff-person ?middle-manager))))

#|

(outranked-by ?middle-manager ?boss)の順番が前に来てしまったことで?middle-managerが全ての人になり、
自分の(outranked-by)をさらにcallしてしまい、無限ループになる。

|#
