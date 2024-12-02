; 階乗マシン
(controller
	  (assign continue (label fact-done)) ;set up final return address
	fact-loop
	  (test (op =) (reg n) (const 1))
	  (branch (label base-case))
	  ;; Set up for the recursive call by saving n and continue.
	  ;; Set up continue so that the computation will continue
	  ;; at after-fact when the subroutine returns.
	  (save continue)
	  (save n)
	  (assign n (op -) (reg n) (const 1))
	  (assign continue (label after-fact))
	  (goto (label fact-loop))
	after-fact
	  (restore n)
	  (restore continue)
	  (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
	  (goto (reg continue)) ;return to caller
	base-case
	  (assign val (const 1))
	  (goto (reg continue)) ;base case: 1! = 1
	  ;return to caller
	fact-done)

#|

(factorial 4)

continue: fact-done

test = 4 1
push fact-done
push 4
n: 3
continue: after-fact

test = 3 1
push after-fact
push 3
n: 2
continue: after-fact

test = 2 1
push after-fact
push 2
n: 1
continue: after-fact

test = 1 1
val: 1
goto: after-fact

(restore n)	n: 2
(restore continue)	after-fact
val: 2
goto: after-fact

(restore n)	n: 3
(restore continue)	after-fact
val: 6
goto: after-fact

(restore n)	n: 4
(restore continue)	fact-done
val: 24
goto: fact-done

|#

; フィボナッチマシン
(controller
		(assign continue (label fib-done))
	  fib-loop
		(test (op <) (reg n) (const 2))
		(branch (label immediate-answer))
		;; Fib(n−1) を求める準備
		(save continue)
		(assign continue (label afterfib-n-1))
		(save n) ; n の古い値を保存
		(assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
		(goto (label fib-loop)) ; 再帰呼び出しの実⾏
	  afterfib-n-1 ; リターン時に Fib(n−1) は val に⼊っている
		(restore n)
		(restore continue)
		;; Fib(n−2) を求める準備
		(assign n (op -) (reg n) (const 2))
		(save continue)
		(assign continue (label afterfib-n-2))
		(save val) ; Fib(n−1) を保存
		(goto (label fib-loop))
	  afterfib-n-2 ; リターン時に Fib(n−2) は val に⼊っている
		(assign n (reg val)) ; n には Fib(n−2) が⼊る
		(restore val) ; val には Fib(n−1) が⼊る
		(restore continue) 
		(assign val ; Fib(n−1) + Fib(n−2)
			(op +) (reg val) (reg n))
		(goto (reg continue)) ; 呼び出し元に戻る、答えはval の中
	  immediate-answer
		(assign val (reg n))
		(goto (reg continue))
	  fib-done)

#|

(fib 5)

continue: fib-done

test < 5 2
push fib-done
continue: afterfib-n-1
push 5
n: 4
goto: fib-loop

test < 4 2
push afterfib-n-1
continue: afterfib-n-1
push 4
n: 3
goto: fib-loop

test < 3 2
push afterfib-n-1
continue: afterfib-n-1
push 3
n: 2
goto: fib-loop

test < 2 2
push afterfib-n-1
continue: afterfib-n-1
push 2
n: 1
goto: fib-loop

test < 1 2
val: 1
goto: afterfib-n-1

(restore n) n: 2
(restore continue) continue: afterfib-n-1
n: 0
push afterfib-n-1
continue: afterfib-n-2
push 1
goto fib-loop

test < 0 2
val: 0
goto afterfib-n-2

n: 0
(restore val) val: 1
(restore continue) continue: afterfib-n-1
val: 1
goto: afterfib-n-1

(restore n) n: 3
(restore continue) continue: afterfib-n-1
n: 1
push afterfib-n-1
continue: afterfib-n-2
push 1
goto fib-loop

test < 1 2
val: 1
goto afterfib-n-2

n: 1
(restore val) 1
(restore continue) afterfib-n-1
val: 2
goto afterfib-n-1

(restore n) n: 4
(restore continue) continue: afterfib-n-1
n: 2
push afterfib-n-1
continue: afterfib-n-2
push 2
goto fib-loop

test < 2 2
push afterfib-n-2
continue afterfib-n-1
push 2
n: 1
goto fib-loop

test < 1 2
val: 1
goto afterfib-n-1

(restore n) n: 2
(restore continue) continue: afterfib-n-2
n: 0
push afterfib-n-2
continue: afterfib-n-2
push 1
goto fib-loop

test < 0 2
val: 0
goto afterfib-n-2

n: 0
(restore val) 1
(restore continue) afterfib-n-2
val: 1
goto afterfib-n-2

n: 1
(restore val) 2
(restore continue) afterfib-n-1
val: 3
goto afterfib-n-1

(restore n) 5
(restore continue) fib-done
n: 3
push fib-done
continue: afterfib-n-2
push 3
goto fib-loop

test < 3 2
push afterfib-n-2
continue: afterfib-n-1
push 3
n: 2
goto fib-loop

test < 2 2
push afterfib-n-1
continue: afterfib-n-1
push 2
n: 1
goto fib-loop

test < 1 2
val: 1
goto afterfib-n-1

(restore n) 2
(restore continue) afterfib-n-1
n: 0
push afterfib-n-1
continue: afterfib-n-2
push 1
goto fib-loop

test < 0 2
val: 0
goto afterfib-n-2

n: 0
(restore val) 1
(restore contiune) afterfib-n-1
val: 1
goto afterfib-n-1

(restore n) 3
(restore contiune) afterfib-n-2
n: 1
push afterfib-n-2
continue: afterfib-n-2
push 1
goto fib-loop

test < 1 2
val: 1
goto afterfib-n-2

n: 1
(restore val) 1
(restore continue) afterfib-n-2
val: 2
goto afterfib-n-2

n: 2
(restore val) 3
(restore continue) fib-done
val: 5
goto fib-done

|#