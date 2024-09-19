; 良問 adjoin-postionだけできなかった

#lang racket

(define (print-variable name value)
  (printf "~a: ~a\n" name value))
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
	; (display "positions: ")(display positions)(newline)
	; (display "k: ")(display k)(newline)
	; 列検知 同じ数字がないか
	; 斜め検知 上左下右
	; '((2 1) (1 2))
	; (2 1)
	(define (safe-col-cross l i) ; i = 7, 6, 5... 2, 1
		(let
			(
				(len (length l))
			)
			(cond
				((<= i 0) #f)
				(else
					(or 
						(= (abs (- (list-ref l 0) (list-ref l (- len i)))) (- len i)); cross
						(= (abs (- (list-ref l 0) (list-ref l (- len i)))) 0); col
						(safe-col-cross l (- i 1))
					)
				)
			)
		)
	)

	(not (safe-col-cross positions (- k 1)))
	; 小さい方から順々にsafeかどうか判定しているので
	; (cond
	; 	((= k 1) #t)
	; 	(else 
	; 		(and
	; 			(not (naname positions (- k 1)))
	; 			(safe? (- k 1) (cdr positions))
	; 		)
	; 	)
	; )
)

; (define (list-back back list k)
; 	(cond
; 		((null? list) nil)
; 		((< (length list) (- k back)) list)
; 		(else (list-back back (cdr list) k))
; 	)
; )

; (define (list-until until list)
; 	(cond
; 		((or (null? list) (< (length list) until) (= until 0)) nil)
; 		(else (cons (car list) (list-until until (cdr list))))
; 	)
; )

(define (queens board-size)
	(define empty-board
		nil
	)
	
	(define (adjoin-position row k rest-of-queens)
		; (display "adjoin-position")(newline)
		; (print-variable "row" row)
		; (print-variable "k" k)
		; (print-variable "rest-of-queens" rest-of-queens)
		; (print-variable "mae" (list-until (- row 1) rest-of-queens))
		; (print-variable "usi" (cons k (list-back (- row 1) rest-of-queens k)))
		; (print-variable "and" (append
		; 	(list-until (- row 1) rest-of-queens)
		; 	(cons k (list-back (- row 1) rest-of-queens k))
		; ))
		(print-variable "ans" (cons row rest-of-queens))
		; (newline)

		; (2 3 1), (1 3 2)
		; rest=(2 1), (1 2) row=2 k=3
		; append (row-1回のリスト) (row-1回cdrして、cons)
		; insert i番目 value list
		; (append
		; 	(list-until (- row 1) rest-of-queens)
		; 	(cons k (list-back (- row 1) rest-of-queens k))
		; )

		; answer
		(cons row rest-of-queens)
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
					; (2 1) (1 2)
					; (3 2 1) (3 1 2)
					; (2 3 1) (2 1 3)
					; (1 2 3) (1 3 2)
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

(queens 4)

; (safe? 8 `(4 2 7 3 6 8 5 1))
; (safe? 8 `(4 2 7 3 6 8 5 7))
; (safe? 8 `(4 2 7 3 6 8 5 5))

; (list-until 4 `(4 2 7 3 6 8 5 1))
; (list-back 1 `() 1)
; (list-until 1 `())
; (null? nil)