#|

code = (
	(define (withdraw amount) (if (>= balance amount)
		(begin (set! balance (- balance amount)) balance)
		"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw) ((eq? m 'deposit) deposit) (else
		(error "Unknown request: MAKE-ACCOUNT" m))))
	dispatch
	)

		|
global->|	make
		|______________________________________________
			↑	↑							  ↑		   ↑
			↑	↑				   			   ↑		↑
			↑ 	widr E3v balance 30			  	↑		↑
			↑	depo E2v balance 90				↑		↑
			↑	mkac E1  balance 50			   mkac E2  balance 100
			〇〇→↑							  〇〇->↑
			↑ 								   ↑
			code→→→→→→→→→→→→→→→→↑

|#