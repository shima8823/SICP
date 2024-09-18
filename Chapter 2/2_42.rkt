#lang racket

(define nil `())

(define (enumerate-interval low high)
	(if (> low high)
		nil
		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence))
		)
	)
)

(define (flatmap proc seq)
	(accumulate append nil (map proc seq)))

(define (remove item sequence)
	(filter
		(lambda (x) (not (= x item)))
		sequence)
)

(define (queens board-size)
	(define empty-board
		nil
	)
	(define (adjoin-position row col rest-of-queens)
		; (append rest-of-queens col)
		(cons col rest-of-queens)
	)
	(define (safe? k positions)
		(display "position: ")(display positions)(newline)
		; 列検知 同じ数字がないか
		; 斜め検知 上左下右
		; '((2 1) (1 2))
		; (2 1)
		(if (= k 1)
			#t
			(not (or
				(= (car positions) (+ (cadr positions) 1))
				(= (car positions) (- (cadr positions) 1))
			))
		)

		
	)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map
							(lambda (new-row)
								(adjoin-position
									new-row
									k
									rest-of-queens
								)
							)
							(enumerate-interval 1 board-size)
						)
					)
					(queen-cols (- k 1))
				)
			)
		)
	)
	(queen-cols board-size)
)

#|

(queens 2) 解なし
(queens 3) 解なし
(queens 4) (2 4 1 3)

|#

(queens 2)