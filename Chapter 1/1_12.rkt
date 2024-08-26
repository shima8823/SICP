#lang racket
(define (pascal-triangle n col)
	(if (or (= col 0) (= col n))
		1
		(+	(pascal-triangle (- n 1) (- col 1))
			(pascal-triangle (- n 1) col)))
)

(pascal-triangle 4 2)