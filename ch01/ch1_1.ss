;;; Answers to exercise 1

;;; ex 1.2
(define ex1_2
  (/ (+ 5 4
		  (- 2
			 (- 3 (+ 6 (/ 4 5)))))
	 (* 3 (- 6 2) (- 2 7))))

;;; ex 1.3
(define (square x)
  (* x x))

(define (min x y)
  (if (< x y)
	  x
	  y))

(define (smallest x y z)
  (min x (min y z)))

(define (sum_of_two_larger x y z)
  (+ (square x) (square y) (square z)
	 (- (square (smallest x y z)))))

;;; ex 1.4
(define (a_plus_abs_b a b)
  ((if (> b 0) + -) a b))

;;; ex 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
	  0
	  y))

;;; Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
				 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; ex 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
		(else else-clause)))

(define (new-sqrt-iter guess x)
  (if (good-enough? guess x)
	  guess
	  (new-sqrt-iter (improve guess x)
				 x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

;;; ex 1.7
(define (relative-good-enough? guess x)
  (< (/ (abs (- (square guess) x)) guess) 0.001))

(define (relative-sqrt-iter guess x)
  (if (relative-good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
				 x)))

(define (relative-sqrt x)
  (relative-sqrt-iter 1.0 x))

;;; Cube Roots by Newton's Method
(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
	  guess
	  (cube-root-iter (cube-root-improve guess x)
				 x)))

(define (cube-root-improve guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (cube-good-enough? guess x)
  (< (/ (abs (- (* guess guess guess) x)) guess) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

