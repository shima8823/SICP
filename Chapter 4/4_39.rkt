#lang sicp

(define (require p) (if (not p) (amb)))
(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items) (an-element-of (cdr items))))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(define (multiple-dwelling)
	(let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5)) (fletcher (amb 1 2 3 4 5)) (miller (amb 1 2 3 4 5)) (smith (amb 1 2 3 4 5)))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(require (not (= (abs (- fletcher cooper)) 1)))
		(require (not (= (abs (- smith fletcher)) 1)))
		(require (not (= cooper 1)))
		(require (> miller cooper))
		(require (not (= baker 5)))
		(require (distinct? (list baker cooper fletcher miller smith)))
		(list (list 'baker baker) (list 'cooper cooper)
			(list 'fletcher fletcher) (list 'miller miller)
			(list 'smith smith))))

#|
制限の順番は

解に影響はない
時間は影響がある。
c, fのような可能な階が少なくなるような制限は前の方で判定して無駄な比較をさけるべきだ。
上記では制限が多いfletcherのpositionを早く決めることによってfletcherが取りうる全ての選択を減らしている。
また、cooperも同様。

|#