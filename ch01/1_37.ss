;; ex 1.37
(define (cont-frac n d k)
  (define (cont-frac-iter result count)
	(if (= count 0)
	    result
		(cont-frac-iter (/ (n count) (+ (d count) result))
						(- count 1))))
  (cont-frac-iter 0 k))

(define (cont-frac2 n d k)
  (if (= k 0)
	  0
	  (/ (n k)
		 (+ (d k) (cont-frac n d (- k 1))))))


(define (phi k)
  (/ 1
	 (cont-frac2 (lambda (i) 1.0)
			 (lambda (i) 1.0)
			 k)))

