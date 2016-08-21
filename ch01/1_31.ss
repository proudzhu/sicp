;; ex 1.31
(define (product term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product2 term a next b)
  (if (> a b)
	  1
	  (* (term a)
		 (product2 term (next a) next b))))

(define (factorial n)
  (define (identify x) x)
  (define (next x) (+ x 1))
  (product identify 1 next n))

(define (pi-product n)
  (define (pi-term x)
	(/ (* (- x 1) (+ x 1)) (* x x)))
  (define (pi-next x) (+ x 2))
  (* 4.0
	 (product2 pi-term 3 pi-next n)))
