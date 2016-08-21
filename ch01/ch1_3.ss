;; sum integers from a through b
(define (sum-integers a b)
  (if (> a b)
	  0
	  (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
	  0
	  (+ (cube a) (sum-cubes (+ a 1) b))))

(define (cube x) (* x x x))

(define (pi-sum a b)
  (if (> a b)
	  0
	  (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; common procedures
(define (sum term a next b)
  (if (> a b)
	  0
	  (+ (term a)
		 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

(define (pi-sum2 a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (inc4 x) (+ x 4))
  (sum pi-term a inc4 b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	 dx))

;; finding roots of equations by half-interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
	    midpoint
		(let ((test-value (f midpoint)))
		  (cond ((positive? test-value)
				 (search f neg-point midpoint))
				((negative? test-value)
				 (search f midpoint pos-point))
				(else midpoint))))))

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
			(error "Values are not of opposite sign" a b)))))

;; find fixed points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

;; procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
			   1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
			   1.0))

;; newton's method
(define (deriv g)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))
(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
	(- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
				  1.0))

;; abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
							average-damp
							1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
							newton-transform
							1.0))
