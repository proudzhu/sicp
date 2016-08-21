;; ex 1.44
(define (smooth f)
  (lambda (x)
	(/ (+ (f (- x dx))
		  (f x)
		  (f (+ x dx)))
	   3.0)))

(define dx 0.00001)

(define (smooth-n f n)
  ((repeated smooth n) f))

(define (repeated f n)
  (if (= n 1)
	  f
	  (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x)
	(f (g x))))

