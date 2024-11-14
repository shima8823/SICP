(rule (big-shot ?person)
	(and
		(job ?person ?job)
		(job ?maybe-boss ?job)
		(not (outranked-by ?person ?maybe-boss))))

; answer http://community.schemewiki.org/?sicp-ex-4.58
(rule (bigshot ?person ?division) 
	(and (job ?person (?division . ?rest)) 
			(or (not (supervisor ?person ?boss)) 
				(and (supervisor ?person ?boss) 
					(not (job ?boss (?division . ?r)))))))