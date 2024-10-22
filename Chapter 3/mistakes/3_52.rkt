; 良問
#lang sicp

(define-syntax cons-stream
	(syntax-rules ()
		((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define-syntax delay
	(syntax-rules ()
		((_ exp) (lambda () exp))))
(define (force delayed-obj)
	(delayed-obj))

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
	(if (stream-null? s)
		the-empty-stream
		(cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
			(stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream
			low
			(stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream)) ; if true、現在の値とstreamのconsを返す。
			(cons-stream
				(stream-car stream)
				(stream-filter
					pred
					(stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
	(stream-for-each display-line s))
(define (display-line x) (newline) (display x))


(define sum 0)
(define (accum x)
	(set! sum (+ x sum))
	sum)

(define seq
	(stream-map
		accum
		(stream-enumerate-interval 1 20)))
; sum

(define y (stream-filter even? seq))
; sum
(define z
	(stream-filter
		(lambda (x)
			(= (remainder x 5) 0))
		seq))
; sum
(stream-ref y 7)
#|
1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136
'(6 10 28 36 66 78 120 136)
indexは0から。

sum=136

出力
136

|#

(display-stream z)

#|
'(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 stream...)

sumはなぜseqを使った計算の後、より大きなsumを記録するのか？
seqがstream-mapですでに計算した後の列を上のように保持しているため。

出力
10
15
45
55
105
120
190
210

|#

(stream-ref seq 2)
sum
#|

output
6
210

|#