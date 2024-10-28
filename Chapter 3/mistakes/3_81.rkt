#lang sicp
(#%require "../stream_basic_components.rkt")
(#%require (only racket/base current-seconds))

(define a 1103515245)
(define b 12345)
(define m (expt 2 31))

(define random-init (remainder (current-seconds) m))
(define (rand-update x)
	(remainder (+ (* a x) b) m))

(define (random-numbers)
	(cons-stream
		random-init
		(stream-map rand-update random-numbers)))

; (define (random input-stream init)
; 	(define res
; 		(cons-stream
; 			init
; 			(stream-map
; 				(lambda (input x)
; 					(cond
; 						((eq? input 'generate)
; 							(rand-update x))
; 						((eq? input 'reset)
; 							(random
; 								(stream-cdr (stream-cdr input-stream))
; 								(stream-car (stream-cdr input-stream))))
; 						(else (error "Unknown instruction: " m))))
; 				input-stream
; 				res)))
; 	res
; 		; (cond
; 		; 	((eq? (stream-car input-stream) 'generate)
; 		; 		)
; 		; 	((eq? (stream-car input-stream) 'reset)
; 		; 		(random-numbers (stream-car (stream-cdr input-stream))))
; 		; 	(else (error "Unknown instruction: " m)))
; )
  
(define (random s init)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream init
        (cond ((eq? (stream-car s) 'generate)
               (random (stream-cdr s) (rand-update init)))
              ((eq? (stream-car s) 'reset) 
               (random (stream-cdr (stream-cdr s)) (stream-car (stream-cdr s))))
              (else 'error)))))

(define input-stream
	(cons-stream 'generate
	 (cons-stream 'generate
	  (cons-stream 'generate
	   (cons-stream 'reset
	    (cons-stream 10
	 	 (cons-stream 'generate
	 	  (cons-stream 'generate
	 	   (cons-stream 'generate
	 	    (cons-stream 'reset
	 		 (cons-stream 10
	 		  (cons-stream 'generate
	 		   (cons-stream 'generate
	 		    (cons-stream 'generate the-empty-stream))))))))))))))

(display-stream (random input-stream (current-seconds)))