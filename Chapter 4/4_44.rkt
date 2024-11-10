#lang sicp

(define (list-ref items n)
	(if (= n 0)
		(car items)
		(list-ref (cdr items) (- n 1))))

(define (require p) (if (not p) (amb)))
(define (an-integer-between low high)
	(require (<= low high))
	(amb low (an-integer-between (+ low 1) high)))

(define (safe? k positions)
	; (display positions) (newline)
	(define (safe-col-cross l i)
		(let ((len (length l)))
			(and (> i 0) 
				(or (= (abs (- (list-ref l 0) (list-ref l (- len i)))) (- len i))
					(= (abs (- (list-ref l 0) (list-ref l (- len i)))) 0)
					(safe-col-cross l (- i 1))))))
	(not (safe-col-cross positions (- k 1))))

(define (queens board-size)
	(define (require-not-duplicate val genereted-list)
		(if (not (null? genereted-list))
			(begin
				(require (not (= val (car genereted-list))))
				(require-not-duplicate val (cdr genereted-list)))))

	(define (enumerate-integer k genereted-list) ; k=queen's board-szie
		(if (= k 0)
			'()
			(let ((val (an-integer-between 1 board-size)))
				(require-not-duplicate val genereted-list)
				(cons val (enumerate-integer (- k 1) (cons val genereted-list))))))

	(let ((try (enumerate-integer board-size '())))
		; borad-size=4,check は
		; 	(safe? 4) (safe? 3) (safe? 2) (safe? 1)
		; と非効率なことしてしまっている。
		; よってambで数字生成->から(require safe?)でiterでincしていく解法がよさそう。 ref http://community.schemewiki.org/?sicp-ex-4.44
		(define (check try) 
			(if (not (null? try))
				(begin
					(require (safe? (length try) try))
					(check (cdr try)))))
		; (display "try ")(display try) (newline)
		(check try)
		try))

(queens 5)
