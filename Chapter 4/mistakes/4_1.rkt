#lang sicp

; (define (list-of-values exps env)
; 	(if (no-operands? exps)
; 		'()
; 		(cons
; 			(eval (first-operand exps) env)
; 			(list-of-values (rest-operands exps) env))))

(define (list-of-values exps env)
	(if (not (no-operands? exps))
		(list
			(eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env))))


; answer
; http://community.schemewiki.org/?sicp-ex-4.1
 (define (list-of-values-lr exps env) 
   (if (no-operands? exps) 
       '() 
       (let ((first (eval (first-operand exps) env))) 
         (let ((rest (list-of-values-lr (rest-operands exps) env))) 
           (cons first rest))))) 
  
 (define (list-of-values-rl exps env) 
   (if (no-operands? exps) 
       '() 
       (let ((rest (list-of-values-rl (rest-operands exps) env))) 
         (let ((first (eval (first-operand exps) env))) 
           (cons first rest))))) 

(list (+ 5 5) (/ 3 3) '())