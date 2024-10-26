#lang sicp
(#%require "../stream_basic_components.rkt")

(define (square x) (* x x))

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream
			(stream-car s1)
			(interleave s2 (stream-cdr s1)))))

(define (pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(stream-map
				(lambda (x) (list (stream-car s) x))
				(stream-cdr t))
			(pairs (stream-cdr s) (stream-cdr t)))))

#|

以下のtriplesだとinterleaveの中でinterleaveしてしまっているので
頻度が片方に傾いていそう。なので実行速度が遅くなっている。

|#

; (define (triples s t u)
; 	(cons-stream
; 		(list (stream-car s) (stream-car t) (stream-car u))
; 		(interleave
; 			(stream-map
; 				(lambda (x) (list (stream-car s) (stream-car t) x))
; 				(stream-cdr u))
; 			(interleave
; 				(stream-map
; 					(lambda (x) (list (stream-car s) (stream-car (stream-cdr t)) x))
; 					(stream-cdr u))
; 				(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

; answer
 (define (triples s t u) 
   (cons-stream 
    (list (stream-car s) 
          (stream-car t) 
          (stream-car u)) 
    (interleave (stream-map (lambda (x) (cons (stream-car s) x)) 
                            (stream-cdr (pairs t u))) 
                (triples (stream-cdr s) 
                         (stream-cdr t) 
                         (stream-cdr u))))) 

(define pythagoras-stream
	(stream-filter
		(lambda (x)
			(let  ((i (car x))
				   (j (cadr x))
				   (k (caddr x)))
				(= (+ (square i) (square j)) (square k))))
		(triples
			integers
			integers
			integers)))

; (define stream-triples-int (triples integers integers integers))
; (display-stream-until 50 stream-triples-int)
(display-stream-until 50 pythagoras-stream)
