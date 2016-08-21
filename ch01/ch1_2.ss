; factorial
;; linear recursion
(define (factorial-rec n)
  (if (= n 1)
	  1
	  (* n (factorial (- n 1)))))

;; iteration
(define (factorial-iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
	  product
      (fact-iter (* counter product)
				 (+ counter 1)
				 max-count)))

;; fib
(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

;; fib linear iteration
(define (fib-iter n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
	  b
	  (fib-iter (+ a b) a (- count 1))))

;; counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
		((or (< amount 0) (= kinds-of-coins 0)) 0)
		(else (+ (cc amount
					 (- kinds-of-coins 1))
				 (cc (- amount
						(first-denomination kinds-of-coins))
					 kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

;; exponentiation
(define (expt-recursion b n)
  (if (= n 0)
	  1
	  (* b (expt-recursion b (- n 1)))))

(define (expt-iteration b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
	  product
	  (expt-iter b
				 (- counter 1)
				 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))

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

(define (prime? n)
  (= n (smallest-divisor n)))
