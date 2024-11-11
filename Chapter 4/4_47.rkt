; 良問 非決定性計算の気をつけなくてはいけないことがわかる。

; Louis
(define (parse-verb-phrase)
	(amb (parse-word verbs)
		(list 'verb-phrase
			(parse-verb-phrase)
			(parse-prepositional-phrase))))
#|
うまく動かない
無限ループになる。

the student with the cat sleeps ...(parse-word with)の時
(the professor lectures to the student with the cat)
-> 3回目の時、無限ループになる。
-> 2回目までは、ambで回せることができたが3回目になるとparse-wordで永遠にambで試し続けてしまう。

amb ('verb-phrase lectures ('prep-phrase to the student)) ('prep-phrase with the cat))
amb ('verb-phrase ('verb-phrase lectures ('prep-phrase to the student)) ('prep-phrase with the cat))
???

元のparse-verb-phraseは試し続けてしまうparse-wordを分けているため、永遠に試すことが起きない。


amb内の式の順番を入れ替えてもうまく動かない。

|#
(define (parse-verb-phrase)
	(define (maybe-extend verb-phrase)
		(amb verb-phrase
			(maybe-extend
				(list 'verb-phrase
					verb-phrase
					(parse-prepositional-phrase)))))
	(maybe-extend (parse-word verbs)))