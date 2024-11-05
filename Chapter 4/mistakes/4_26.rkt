#lang sicp

(define (unless condition usual-value exceptional-value)
	(if condition exceptional-value usual-value))

; else 派生？
(define (unless condition usual-value exceptional-value)
	(cond (condition exceptional-value)
		  (else usual-value)))

#|

特殊形式とはどういうことだろう?こういうこと？
(unless '(condition) '(usual) '(excep))

利点
それなら手続きの方が高階手続きで通常と同じように使えるので間違いが減る。
手順として持っていると便利かも？

|#

; そもそもmetacircularに組み込む問題だった
; meteorgan from http://community.schemewiki.org/?sicp-ex-4.26
;; add this code in eval 
((unless? expr) (eval (unless->if expr) env)) 
  
;; unless expression is very similar to if expression. 
(define (unless? expr) (tagged-list? expr 'unless)) 
(define (unless-predicate expr) (cadr expr)) 
(define (unless-consequnce expr) 
(if (not (null? (cdddr expr))) 
	(cadddr expr) 
	'false)) 
(define (unless-alternative expr) (caddr expr)) 

(define (unless->if expr) 
	(make-if (unless-predicate expr) (unless-consequence expr) (unless-alternative expr))) 