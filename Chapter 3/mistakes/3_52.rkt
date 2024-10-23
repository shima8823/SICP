; 良問
#lang sicp

(define-syntax cons-stream
	(syntax-rules ()
		((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (memo-proc proc)
	(let ((already-run? false) (result false))
		(lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
					   (set! already-run? true)
						result)
				result))))

(define-syntax delay
	(syntax-rules ()
		; ((_ exp) (memo-proc (lambda () exp)))))
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

; sum = 1
; seq = (1 stream...)
; sum

(define y (stream-filter even? seq))
; sum = 6
sum

(define z
	(stream-filter
		(lambda (x)
			(= (remainder x 5) 0))
		seq))
; seq = 1 stream...
; 8(6+2) 11 15
; sum = 15
sum

(stream-ref y 7)
#|
1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136
sum = 15: (set! sum (+ x sum))
19(4+15) 24 30 37 45 54 64 75 87 100 114 129 145 162
'(6 24 30 54 64 100 114 162)
indexは0から。

sum=162

出力
162

|#

(display-stream z)

#|
sum = 162
167(5+162) 173 180 188 197 207 218 230 243 257 272 288 305 323 342 362 |21以上 383 405
'(15 180 230 305)

出力
15
180
230
305done

|#

; sum = 362

(stream-ref seq 2)
#|

364 367

output
367

|#