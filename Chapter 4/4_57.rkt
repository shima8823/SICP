(rule (replace ?person-1 ?person-2)
	(and (not (same ?person-1 ?person-2))
		(or (and
				(job ?person-1 ?job)
				(job ?person-2 ?job))
			(and
				(job ?person-1 ?job-1)
				(job ?person-2 ?job-2)
				(can-do-job ?job-1 ?job-2)))))

; a
(replace ?person (Fect Cy D))

; b
(and (salary ?person ?amount)
	(salary ?me ?my-amount)
	(lisp-value > ?amount ?my-amount)
	(replace ?person ?me))
