; 良問

#lang sicp
(#%require "./stream_basic_components.rkt")

(define (make-zero-crossings input-stream last-value)
	(cons-stream
		(sign-change-detector
			(stream-car input-stream)
			last-value)
		(make-zero-crossings
			(stream-cdr input-stream)
			(stream-car input-stream))))

(define (smooth stream)
	(define (average x y) (/ (+ x y) 2))
	(define (smooth-iter s)
		(let ((first (stream-car s))
			(second (stream-car (stream-cdr s))))
			(cons-stream
				(average first second)
				(smooth-iter (stream-cdr s)))))
	(smooth (cons-stream 0 (cons-stream 0 stream))))
; コメントアウトされている方のmake-zero-crossingsを使用した場合

; http://community.schemewiki.org/?sicp-ex-3.76
; mapを使った方が簡単
(define (smooth s) 
   (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2)) 
               (cons-stream 0 s) 
               s)) 

; 3_74でやったものを使う
 (define (make-zero-crosssings input-stream smooth) 
   (let ((after-smooth (smooth input-stream))) 
     (stream-map sign-change-detector 
                 after-smooth 
                 (cons-stream 0 after-smooth)))) 

; (define (make-zero-crossings input-stream avpt-stream)
; 	(let ((last-avpt (stream-car avpt-stream))
; 		  (avpt (stream-car (stream-cdr avpt-stream))))
; 		(cons-stream
; 			(sign-change-detector
; 				avpt last-avpt)
; 			(make-zero-crossings
; 				(stream-cdr input-stream) (stream-cdr avpt-stream)))))

