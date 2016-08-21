;; ex 1.38
(define (cont-frac n d k)
  (define (cont-frac-iter result count)
	(if (= count 0)
	    result
		(cont-frac-iter (/ (n count) (+ (d count) result))
						(- count 1))))
  (cont-frac-iter 0 k))

(define (d k)
  (if (<= k 2)
	  k
	  (* (/ 2 3) (+ k 1))))

(define (n k) 1.0)

(define e
  (+ 2
	 (cont-frac n d 100)))
