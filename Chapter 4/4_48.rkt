; 重文

#lang sicp

(define (memq item x)
	(cond ((null? x) false)
		  ((eq? item (car x)) x)
		  (else (memq item (cdr x)))))

(define (require p) (if (not p) (amb)))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define conjunctions '(conj and but so for or))

(define (parse-sentence)
	(list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(define (parse-compound-sentence)
	(define (maybe-extend sentence)
		(amb sentence
			(maybe-extend
				(list 'compound-sentence
					sentence
					(parse-word conjunctions)
					(parse-compound-sentence)))))
	(maybe-extend (parse-sentence)))

(define (parse-simple-noun-phrase)
	(list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))

(define (parse-noun-phrase)
	(define (maybe-extend noun-phrase)
		(amb noun-phrase
			(maybe-extend
				(list 'noun-phrase
					noun-phrase
					(parse-prepositional-phrase)))))
	(maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
	(define (maybe-extend verb-phrase)
		(amb verb-phrase
			(maybe-extend
				(list 'verb-phrase
					verb-phrase
					(parse-prepositional-phrase)))))
	(maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
	(list 'prep-phrase
		(parse-word prepositions)
		(parse-noun-phrase)))

(define (parse-word word-list)
	(require (not (null? *unparsed*)))
	(require (memq (car *unparsed*) (cdr word-list)))
	(let ((found-word (car *unparsed*)))
		(set! *unparsed* (cdr *unparsed*))
		(list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
	(set! *unparsed* input)
	(let ((sent (parse-compound-sentence)))
		(require (null? *unparsed*)) sent))

(parse '(the cat eats)) ; 1
(parse '(the student with the cat sleeps in the class)) ; 1
(parse '(the professor lectures to the student with the cat)) ; 2
(parse '(the professor lectures to the student in the class with the cat)) ; 5
(parse '(the professor lectures to the student and the cat eats))