#lang sicp
(#%require "./stream_basic_components.rkt")

(define (square x) (* x x))

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

(define (weighted-pairs s t weight)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map
				(lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
			weight)))

(define (merge-weighted s1 s2 weight)
	; (display s1)(display " s2 ")(display s2)(newline)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2))
				  (s1car-w (weight (stream-car s1)))
				  (s2car-w (weight (stream-car s2))))
				(cond
					((< s1car-w s2car-w)
						; (display s1car-w)(display " s2 ")(display s2car)(newline)
						(cons-stream
							s1car
							(merge-weighted (stream-cdr s1) s2 weight)))
					(else
						(cons-stream
							s2car
							(merge-weighted s1 (stream-cdr s2) weight))))))))

(define (sum-square x)
	(let ((i (car x))
		  (j (cadr x)))
		(+ (* i i) (* j j))))

(define sum-square-order-stream
	(weighted-pairs
		integers
		integers
		(lambda (x) (sum-square x))))

(display-stream-until 20 sum-square-order-stream)


(define (stream-sum-square-3 stream)
	(if (stream-null? stream) the-empty-stream
		(let ((first  (stream-car stream))
			  (second (stream-car (stream-cdr stream)))
			  (third  (stream-car (stream-cdr (stream-cdr stream)))))
			(if (= (sum-square first) (sum-square second) (sum-square third))
				(begin (display first)(display " ")(display second)(display " ")(display third)(display " = ")
				(cons-stream
					(sum-square first)
					(stream-sum-square-3
						(stream-cdr (stream-cdr (stream-cdr stream))))))
				(stream-sum-square-3 (stream-cdr stream))))))

#|

streamは無限である前提。
最初のstreamから3つの値を取得して比較
trueならば値をstreamに追加して、3つずらして比較再起
3つずらしで問題ない理由は、そもそも
	⼆つの平⽅数の和として表す⽅法が
	三通りあるすべての数のストリームを生成することについて問うているため。
	
falseならば1つずらして比較再起

|#

(define stream-like-ramanujan
	(stream-sum-square-3 sum-square-order-stream))
(display-stream-until 20 stream-like-ramanujan)

