(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert!
	(rule (son ?m ?s)
		(and (wife ?m ?w)
			 (son  ?w ?s))))

(assert!
	(rule ((grandson) ?g ?s)
		(and (son ?g ?p)
			 (son ?p ?s))))

((grandson) ?p ?c)


#|

DEBUG

((great grandson) ?g ?ggs)
(?relationship Adam Irad)

|#
