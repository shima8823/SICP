
(define (make-primitive-exp exp machine labels)
	(cond
		((constant-exp? exp)
			(let ((c (constant-exp-value exp)))
				(lambda () c)))
		((label-exp? exp)
			(let ((insts (lookup-label labels
					(label-exp-label exp))))
			(lambda () insts)))
		((register-exp? exp)
			(let ((r (get-register machine (register-exp-reg exp))))
				(lambda () (get-contents r))))
		(else (error " Unknown expression type : ASSEMBLE " exp))))

(define (make-operation-exp exp machine labels operations)
	(let ((op (lookup-prim (operation-exp-op exp) operations))
		  (aprocs
			(map (lambda (e)
				(if (label-exp? e) ; change
					(error "Label can't operate " e) ; change
					(make-primitive-exp e machine labels)))
			(operation-exp-operands exp))))
		(lambda ()
			(apply op (map (lambda (p) (p)) aprocs)))))