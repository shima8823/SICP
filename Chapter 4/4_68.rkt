(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	  		   (append-to-form ?v ?y ?z)))

#|
;;; Query input:
(append-to-form ?x ?y (a b c d))
;;; Query results:
(append-to-form () (a b c d) (a b c d))
(append-to-form (a) (b c d) (a b c d))
(append-to-form (a b) (c d) (a b c d))
(append-to-form (a b c) (d) (a b c d))
(append-to-form (a b c d) () (a b c d))
|#

(define (reverse l)
	(if (null? l)
		(list)
		(append (reverse (cdr l)) (list (car l)))))



(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	  		   (append-to-form ?v ?y ?z)))

(assert! (rule (reverse (?last . ()) (?last . ()))))
; (assert! (rule (reverse () ())))
(assert!
(rule (reverse (?order . ?o-rest) (?reverse . ?r-rest))
	(and
		(reverse ?o-rest ?reverse)
	  	(append-to-form (?order) () (?r-rest))
	)
)
)

(reverse (1 2) ?x)
#|
(reverse (1 . (2 3)) ?x)
	(reverse (2 . (3)) ?x)
		(reverse (3 . ()) ?x)
			(append-to-form () 3 ?x)
		(append-to-form (3) 2 ?x)
	(append-to-form (3 2) 1 ?reverse)
(append-to-form (3 2 1) () ?reverse)

(append-to-form (3) (1 2) (3 1 2))
(append-to-form (3 2) (1) (3 2 1))
(append-to-form (3 2 1) () (3 2 1))

(append-to-form (1 2 3) () ?x)

DEBUG

(reverse (1 2 3) ?x)
(reverse ?x (1 2 3))

(assert!
(rule (reverse (?order . ?o-rest) (?reverse . ?r-rest))
	(and
		(reverse ?o-rest ?reverse)
	  	(append-to-form (?order) () (?r-rest))
	)
)
)


|#
