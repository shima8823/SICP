#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (lookup given-key set-of-records)
	(cond
		((null? (entry set-of-records)) false)
		((= given-key (key (entry set-of-records))) (car set-of-records))
		((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
		(else (lookup given-key (right-branch set-of-records)))))
