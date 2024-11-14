;a
(and (supervisor ?person (Bitdiddle Ben))
	(address ?person ?where))
;b
(and (salary ?person ?amount)
	(salary (Bitdiddle Ben) ?ben-salary)
	(lisp-value < ?amount ?ben-salary))
;c
(and (not (job ?person (computer . ?type)))
	(supervisor ?person ?boss)
	(job ?boss ?work))
