;; ex 1.16
(define (fast-expt-iter b n)
  (define (iter a b n)
	(cond ((= n 0) a)
		  ((even? n) (iter a (square b) (/ n 2)))
		  (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

