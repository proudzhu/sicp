;; ex 1.18
(define (mult-iter a b)
  (define (iter add a b)
	(cond ((= b 0) add)
		  ((even? b) (iter add (double a) (halve b)))
		  (else (iter (+ add a) (double a) (halve b)))))
  (iter 0 a b))
