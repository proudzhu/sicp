;; ex 1.40
(define (cubic a b c)
  (lambda (x)
	(+ (* x x x)
	   (* a x x)
	   (* b x)
	   c)))

(define (cubic-root a b c)
  (newtons-method (cubic a b c) 1))

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

