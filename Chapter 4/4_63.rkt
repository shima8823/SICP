(rule (wheel ?person)
	(and (supervisor ?middle-manager ?person)
		 (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
	(or (supervisor ?staff-person ?boss)
		(and (supervisor ?staff-person ?middle-manager)
			 (outranked-by ?middle-manager ?boss))))

; “もしSがfの息⼦であり、かつ、fがGの息⼦ならば、SはGの孫である”


(rule (grandson ?g ?s)
	(and (son ?f ?s)
		 (son ?g ?f)))

; “もしWがMの妻であり、かつ、SがWの息⼦ならば、SはMの息⼦である”
(rule (son ?m ?s)
	(and (wife ?m ?w)
		 (son  ?w ?s)))
