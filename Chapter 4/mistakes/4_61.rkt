; 良問 論理プログラムを理解しやすい

(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
	  (?x next-to ?y in ?z))

(?x next-to ?y in (1 (2 3) 4))
#| output

(1 next-to (2 3) in (1 (2 3) . 4))
((2 3) next-to 4 in ((2 3) 4))

; answer
順番は実装依存？
埋まっているクエリはそのまま。
(1 next-to (2 3) in (1 (2 3) 4)))
((2 3) next-to 4 in (1 (2 3) 4)))

|#

(?x next-to 1 in (2 1 3 1))
#| output

(2 next-to 1 in (2 1 . 3 1))
(3 next-to 1 in (3 1))

; answer
順番は実装依存？
埋まっているクエリはそのまま。
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

|#
