;; ex 1.36
;; find fixed points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(newline)
	(display guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(newline)
	(display guess)
	(let ((next (average guess (f guess))))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

(define f
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
			   4.0))

(define f2
  (fixed-point2 (lambda (x) (/ (log 1000) (log x)))
			    4.0))

