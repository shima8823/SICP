#lang racket

(define (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
				(encode (cdr message) tree))))

(define (encode-symbol symbol tree)
	(define (encode-iter tree bit)
		(cond ((null? tree) '())
			  ((leaf? tree)
			  	(if (eq? symbol (car (symbols tree)))
					bit
					'()))
			  (else 
				(let ((left (encode-iter (left-branch tree) (append bit '(0)))))
					(if (not (null? left))
						left
						(encode-iter (right-branch tree) (append bit '(1))))
				)
			))
	)
	(encode-iter tree '())
)

#|

頻度
最大 
左にある場合と右にある場合がある
最速 O(1)
最おそ O(n)

最低 
最速 O(n)
最おそ O(n^2) 例2_71

|#