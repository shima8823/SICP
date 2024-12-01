(controller
	test-b
		(test (op =) (reg b) (const 0))
		(branch (label gcd-done))
		(assign t (op rem) (reg a) (reg b))
		(assign a (reg b))
		(assign b (reg t))
		(goto (label test-b))
	gcd-done)

(controller
		(assign p (const 1))
		(assign c (const 1))
	test-b
		(test (op >) (reg c) (reg n))
		(branch (label factorial-done))
		(assign p (op mul) (reg c) (reg p))
		(assign c (op add) (reg c) (const 1))
		(goto (label test-b))
	factorial-done)