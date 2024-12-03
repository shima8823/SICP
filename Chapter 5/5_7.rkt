(define expt-machine
	(make-machine
		'(n b val continue)
		(list (list '= =) (list '- -) (list '* *))
		'((assign continue (label expt-done))
		expt-loop
		  (test (op =) (reg n) (const 0))
		  (branch (label const-answer))
		  (assign n (op -) (reg n) (const 1))
		  (save continue)
		  (assign continue (label after-expt))
		  (goto (label expt-loop))
		after-expt
		  (restore continue)
		  (assign val (op *) (reg b) (reg val))
		  (goto (reg continue))
		const-answer
		  (assign val (const 1))
		  (goto (reg continue))
		expt-done)))

(define expt-iter-machine
	(make-machine
		'(n b counter product)
		(list (list '= =) (list '- -) (list '* *))
		'((assign counter (reg n))
	  	  (assign product (const 1))
		expt-iter
		  (test (op =) (reg counter) (const 0))
		  (branch (label expt-done))
		  (assign counter (op -) (reg counter) (const 1))
		  (assign product (op *) (reg b) (reg product))
		  (goto (label expt-iter))
		expt-done)))
