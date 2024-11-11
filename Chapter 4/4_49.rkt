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

(define (parse-sentence)
	(list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

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
	(define (expansion-words words)
		(require (not (null? words)))
		(amb (car words)
			(expansion-words (cdr words))))

	(require (not (null? *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) (expansion-words (cdr word-list))))

(define *unparsed* '())
(define (parse input)
	(set! *unparsed* input)
	(let ((sent (parse-sentence)))
		(require (null? *unparsed*)) sent))

(parse '(the cat eats))

#|

(sentence (simple-noun-phrase (article the) (noun student)) (verb studies))
(sentence (simple-noun-phrase (article the) (noun student)) (verb lectures))
(sentence (simple-noun-phrase (article the) (noun student)) (verb eats))

...
(sentence (simple-noun-phrase (article a) (noun class)) (verb eats))
(sentence (simple-noun-phrase (article a) (noun class)) (verb sleeps))

|#