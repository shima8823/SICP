; 良問 ruleの順番で答えが得られる

(rule (last-pair (?x . ()) ?x))
(rule (last-pair (?val . ?rest) ?x)
	  (last-pair ?rest ?x))


; answer > 空でないリストの最後の要素を含むリストを返すものである。
(rule (last-pair (?val . ?rest) (?x))
	  (last-pair ?rest (?x)))
(rule (last-pair (?x) (?x)))

#|

DEBUG TEST

(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))

(last-pair ?x (3)) ;->動作するはず

; answer
最後のクエリはruleの順番を変えても無限ループになるが、answer順の方が無限ストリームになる。

|#
