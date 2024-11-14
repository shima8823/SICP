; (meeting accounting (Monday 9am))
; (meeting administration (Monday 10 am))
; (meeting computer (Wednesday 3pm))
; (meeting administration (Friday 1pm))
; (meeting whole-company (Wednesday 4pm))

; a
(meeting ?type (Friday . ?time))

; b
(rule (meeting-time ?person ?day-and-time)
	(and	
		(job ?person (?job . ?type))
		(or 
			(meeting ?job ?day-and-time)
			(meeting whole-company ?day-and-time))))

; c printされないかも？まあいいでしょう
(meeting-time (Hacker Alyssa P) (Wednesday . ?time))
