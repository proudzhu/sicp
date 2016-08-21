;; ex 1.32
(define (product term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* result (term a)))))
  (iter a 1))

(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
	    result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	    result
		(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product2 term a next b)
  (accumulate * 1 term a next b))

(define (sum2 term a next b)
  (accumulate + 0 term a next b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (inc4 x) (+ x 4))
  (* 8
	 (sum2 pi-term a inc4 b)))

(define (pi-product n)
  (define (pi-term x)
	(/ (* (- x 1) (+ x 1)) (* x x)))
  (define (pi-next x) (+ x 2))
  (* 4.0
	 (product2 pi-term 3 pi-next n)))
