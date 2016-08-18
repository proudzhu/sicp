;; ex 1.12
(define (psacal-triangle i j)
  (cond ((= i 0) 1)
		((= j 0) 1)
		((= j i) 1)
		(else (+ (psacal-triangle (- i 1) (- j 1))
				 (psacal-triangle (- i 1) j)))))

