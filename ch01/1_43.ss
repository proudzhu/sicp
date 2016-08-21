;; ex 1.43
(define (repeated f n)
  (if (= n 1)
	  f
	  (compose f (repeated f (- n 1)))))

(define (repeated-iter f n)
  (define (iter k result)
	(if (= k 1)
	    result
		(iter (- k 1) (lambda (x) (f (result x))))))
  (iter n f))

(define (compose f g)
  (lambda (x)
	(f (g x))))

(define (square x)
  (* x x))

((repeated square 2) 5)
;; 49
