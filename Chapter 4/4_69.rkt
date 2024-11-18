; 良問 It's Funny!
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

(assert!
	(rule (last-pair (?val . ?rest) (?x))
		  (last-pair ?rest (?x))))
(assert!
	(rule (last-pair (?x) (?x))))

(assert!
	(rule ((great . ?rel) ?x ?y)
		(and (son ?x ?p)
			 (?rel ?p ?y)
			 (last-pair ?rel (grandson)))))

((great grandson) Adam ?c)
((great great great great great grandson) ?g ?s)

((great grandson) ?g ?ggs)
(?relationship Adam Irad)
