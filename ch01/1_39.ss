;; ex 1.39
(define (cont-frac n d k)
  (define (cont-frac-iter result count)
	(if (= count 0)
	    result
		(cont-frac-iter (/ (n count) (+ (d count) result))
						(- count 1))))
  (cont-frac-iter 0 k))

(define (tan-cf x k)
  (cont-frac (lambda (i)
			   (if (= i 1) x (- (square x))))
			 (lambda (i)
			   (- (* 2 i) 1))
			 k))
