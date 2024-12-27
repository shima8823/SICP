; http://community.schemewiki.org/にシンプルなコードとしてコミットしたい

#lang sicp

(#%require "util_metacircular.rkt")

(define (find-variable var compile-time-env)
	(define (env-loop frame-count env)
		(define (scan displacement-count vars)
			(cond
				((null? vars)
					(env-loop (+ frame-count 1) (enclosing-environment env)))
				((eq? var (car vars)) (list frame-count displacement-count))
				(else (scan (+ displacement-count 1) (cdr vars)))))
		(if (eq? env the-empty-environment)
			'not-found
			(let ((frame (first-frame env)))
				(scan
					0
					frame))))
	(env-loop 0 compile-time-env))

(find-variable 'c '((y z) (a b c d e) (x y)))
; (1 2)
(find-variable 'x '((y z) (a b c d e) (x y)))
; (2 0)
(find-variable 'w '((y z) (a b c d e) (x y)))
; not-found