;; ex 1.17
(define (mult_org a b)
  (if (= b 0)
	  0
	  (+ a (mult_org a (- b 1)))))

(define (mult a b)
  (cond ((= b 0) 0)
		((even? b) (mult (double a) (halve b)))
		(else (+ a (mult (double a) (halve b))))))

(define (double x) (+ x x))
(define (halve x) (quotient x 2))

