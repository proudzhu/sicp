;; ex 1.30
(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
	    result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (inc4 x) (+ x 4))
  (sum pi-term a inc4 b))

