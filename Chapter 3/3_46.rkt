#lang racket

(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val))
		serialized-p)))

(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond
				((eq? m 'acquire)
					(if (test-and-set! cell)
						(the-mutex 'acquire))) ; retry
				((eq? m 'release)
					(clear! cell))))
		the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
	(if (car cell)
		#t
		(begin (set-car! cell #t) #f)))

#|

	Peter				mutex				Paul
|	test-and-set!							test-and-set!
|	#t									
|						Peter				#t
|					Peter,Paul					
|


|#