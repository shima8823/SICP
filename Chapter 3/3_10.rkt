#lang racket

; a
(define (make-withdraw initial-amount)
	(let ((balance initial-amount))
		(lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
					balance)
				"Insufficient funds"))))

; b
(define (make-withdraw initial-amount)
	((lambda (balance)
		(lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
					balance)
				"Insufficient funds"))))
	initial-amount)

#|

code = (lambda (amount)
			(if (>= balance amount)
				(begin (set! balance (- balance amount))
					balance)
				"Insufficient funds"))

		|
global->|	make
		|______________________________________________
			↑	↑							  ↑	   ↑
			↑	E3 balance: 50				   ↑	↑
			↑ 			↑amount50			  ↑		↑
			↑	E2 balance: 100				   ↑E5	balance: 100
			↑	E1 initial-amount			   ↑E4	initial
			〇〇→↑		↑					  〇〇->↑
			↑ 			amount50			   ↑
			code→→→→→→→→→→→→→→→→↑

|#