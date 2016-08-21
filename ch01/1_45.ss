;; ex 1.45
(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x y y))
							average-damp
							1.0))

(define (nth-root n x)
  (define (log2 n) (/ (log n) (log 2)))
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
							(repeated average-damp (floor (log2 n)))
							1.0))

(define (repeated f n)
  (if (= n 1)
	  f
	  (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x)
	(f (g x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

