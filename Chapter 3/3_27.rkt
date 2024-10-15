#lang racket
(require r5rs) ; set-(car|cdr)!

(define (make-table) (list '*table*))

(define (lookup key table)
	(let ((record (assoc key (cdr table))))
		(if record
			(cdr record)
			false)))

(define (assoc key records)
	(cond
		((null? records) false)
		((equal? key (caar records)) (car records))
		(else (assoc key (cdr records)))))

(define (insert! key value table)
	(let ((record (assoc key (cdr table))))
		(if record
			(set-cdr! record value)
			(set-cdr! table (cons (cons key value) (cdr table)))
			))
	'ok)

(define (fib n)
	(cond
		((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1)) (fib (- n 2))))))

(define (memoize f)
	(let ((table (make-table)))
		(lambda (x)
			(let ((previously-computed-result (lookup x table)))
				(or previously-computed-result
					(let ((result (f x)))
						(insert! x result table)
						result))))))

(define memo-fib
	(memoize
		(lambda (n)
			(cond
				((= n 0) 0)
				((= n 1) 1)
				(else (+ (memo-fib (- n 1)) (memo-fib (- n 2))))))))

(define not-memo-fib (memoize fib))

(memo-fib 40)
(not-memo-fib 40)
#|

				(memo-fib 3)
		(memo-fib 2) (memo-fib 1)
(memo-fib 1) (memo-fib 0)

なぜnに比例するステップ数で計算できるか？
一回計算したnはテーブルに保存してあるので、そのテーブルからの検索タイム(今回はO(n),n個のリストから見つける)で計算できる。

単に memo-fib を (memoize fib)と定義したnot-memo-fibは上手くいかない
なぜならfibはテーブルから検索すらしないから。(let ((result (f x))) この部分
逆にmemo-fibではfibの中でmemo-fibを呼びmemoizeによって定義されたテーブルを検索している。

テーブルが再定義されないのは memo-fibが以下のように展開されてletで定義した局所変数は最初にしか解釈されないから。
(lambda (x)
	(let ((previously-computed-result (lookup x table)))
		(or previously-computed-result
			(let ((result (f x)))
				(insert! x result table)
				result))))

環境図
https://sicp.iijlab.net/solution/ex3.3.html#ex3.25

|#