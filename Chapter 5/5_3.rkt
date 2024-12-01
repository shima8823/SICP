(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess)
		(average guess (/ x guess)))
	(define (sqrt-iter guess)
		(if (good-enough? guess)
			guess
			(sqrt-iter (improve guess))))
	(sqrt-iter 1.0))

(controller
	test-g
	  (good-enough? (reg g))
	  (branch (label sqrt-done))
	  (assign g (improve g))
	  (goto (label test-g))
	sqrt-done)

(controller
	  (assign x (op read))
	  (assign g (const 1.0))
	test-g
	  (assign t (op *) (reg g) (reg g))
	  (assign t (op -) (reg t) (reg x))
	; abs
	  (test (op >=) (reg t) (const 0))
	  (branch (label test-g-2))
	  (assign t (op *) (reg t) (const -1))
	; 
	test-g-2
	  (test (op <) (reg t) (const 0.0001))
	  (branch (label sqrt-iter-done))
	  (assign d (op /) (reg x) (reg g))
	  (assign t (op +) (reg g) (reg d))
	  (assign t (op /) (reg t) (const 2))
	  (assign g (reg t))
	  (goto (label test-g))
	sqrt-iter-done)

