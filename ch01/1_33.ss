;; ex 1.33
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
	(if (> a b)
	    result
		(iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate filtered combiner null-value term a next b)
  (define (iter a result)
	(cond ((> a b) result)
		  ((not (filtered a)) (iter (next a) result))
		  (else (iter (next a) (combiner result (term a))))))
  (iter a null-value))

(define (true x) 1)

(define (product2 term a next b)
  (filtered-accumulate true * 1 term a next b))

(define (sum2 term a next b)
  (filtered-accumulate true + 0 term a next b))

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

(define (sum-primes-square a b)
  (define (next x) (+ x 1))
  (filtered-accumulate prime? + 0 square a next b))

(define (product-integers-prime n)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (define (filtered x) (= (gcd x n) 1))
  (filtered-accumulate filtered * 1 identity 1 next n))

;; testing for primality
;;; searching for divisors
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (if (= n 1)
	  #f
	  (= n (smallest-divisor n))))

