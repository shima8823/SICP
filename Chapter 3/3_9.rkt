#lang racket

#|

recursive
		|
global->|
		|_________________________________
(factrial)	↑
	E1	(if () 1 (*nf))

			G
			↑
	E2	(if () 1 (*nf))

			G
			↑
	E6	(if () 1 (*nf))
	...

iterate
		|
global->|
		|_________________________________
			↑
	E1	(factorial) (n, (fact-iter))

			G
			↑
	E2	(fact-iter) (p c m, ...(fact-iter)...)

			G
			↑
	E8	(fact-iter) (p c m, ...(fact-iter)...)
	...

|#
