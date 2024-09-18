#lang racket

(define nil `())

(define (abs x)
	(cond ((< x 0) (- x))
		(else x)))

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define (length items)
	(if (null? items)
		0
		(+ 1 (length (cdr items)))))

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
(define (safe? k positions)
	(display "positions: ")(display positions)(newline)
	(display "k: ")(display k)(newline)
	; 列検知 同じ数字がないか
	; 斜め検知 上左下右
	; '((2 1) (1 2))
	; (2 1)
	(define (naname l i) ; i = 7, 6, 5... 2, 1
		(let
			(
				(len (length l))
			)
			(cond
				((<= i 0) #f)
				(else
					(or 
						(= (abs (- (list-ref l 0) (list-ref l (- len i)))) (- len i))
						(naname l (- i 1))
					)
				)
			)
		)
	)

	(cond
		((= k 1) #t)
		(else 
			(and
				(not (naname positions (- k 1)))
				(safe? (- k 1) (cdr positions))
			)
		)
	)
)

(define (queens board-size)
	(define empty-board
		nil
	)
	(define (adjoin-position row col rest-of-queens)
		; (append rest-of-queens col)
		(cons col rest-of-queens)
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

(safe? 8 `(4 2 7 3 6 8 5 1))
(safe? 8 `(4 2 7 3 6 8 5 7))