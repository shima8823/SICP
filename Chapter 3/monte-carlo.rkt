#lang racket

(define (rand) (random 10000))

(define (estimate-pi trials)
	(sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
	(= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0) 
				(/ trials-passed trials))
			  ((experiment)
				(iter (- trials-remaining 1)
					(+ trials-passed 1)))
			  (else
				(iter (- trials-remaining 1) trials-passed ))))
	(iter trials 0))


; a, b, m from ChatGPT prompt: "rand-update を実装する一般的な方法としては、a, b, m を適切に選ばれた整数とし て、x を ax + b modulo m に更新するというものがありますが適切なa, b, mは具体的にどんな数字でしょうか？"
(define a 1103515245)
(define b 12345)
(define m (expt 2 31))
(define random-init (remainder (current-seconds) m))

(define (rand-update x)
	(remainder (+ (* a x) b) m))

(define (estimate-pi-without-assignment trials)
	(sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
	(define (iter trials-remaining trials-passed x)
		(let ((x1 (rand-update x)))
			(let ((x2 (rand-update x1)))
				(cond ((= trials-remaining 0)
						(/ trials-passed trials))
					  ((= (gcd x1 x2) 1)
						(iter (- trials-remaining 1)
							  (+ trials-passed 1) x2))
					  (else
						(iter (- trials-remaining 1)
							  trials-passed x2))))))
	(iter trials 0 initial-x))

(estimate-pi 1000)
(estimate-pi-without-assignment 100000)