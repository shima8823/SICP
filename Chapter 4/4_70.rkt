(define (add-assertion! assertion)
	(store-assertion-in-index assertion)
	(let ((old-assertions THE-ASSERTIONS))
		(set! THE-ASSERTIONS
			(cons-stream assertion old-assertions))
		'ok))

(define (add-assertion! assertion)
	(store-assertion-in-index assertion)
	(set! THE-ASSERTIONS
		(cons-stream assertion THE-ASSERTIONS))
	'ok)

#|

(cons-stream assertion (cons-stream assertion (cons-stream assertion ... THE-ASSERTIONS)))
になってしまうから。

|#