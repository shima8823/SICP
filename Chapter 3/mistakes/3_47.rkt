; skip

; https://sicp.iijlab.net/solution/ex3.4.html
(define (make-semaphore n)
  (let ((mutex (make-mutex)))
    (define (process-acquire)
      (mutex 'acquire)
      (if (n > 0)                   ;
          (set! n (- n 1))          ;n を変更するのでmutexで保護する. 
          (begin (mutex 'release) (process-acquire)))
                                    ;先に進めない時はmutexを解除
      (mutex 'release))             ;セマフォ処理が済んだ時もmutexを解除
    (define (process-release)
      (mutex 'acquire)
      (set! n (+ n 1))
      (mutex 'release))
    (define (dispatch m)
      (cond ((eq? m 'acquire) (process-acquire))
            ((eq? m 'release) (process-release))
            (else (error "Unknown request SEMAPHORE" dispatch))))
  dispatch))

(define (P semaphore)
  (semaphore 'acquire))

(define (V semaphore)
  (semaphore 'release))