;; ex 1.11
;;; recursive
(define (f-1-11-rec n)
  (if (< n 3)
	  n
	  (+ (f-1-11-rec (- n 1))
		 (* (f-1-11-rec (- n 2)) 2)
		 (* (f-1-11-rec (- n 3)) 3))))

;;; iterative
(define (f-1-11-iter n)
  (f-1-11-iter-impl 0 1 2 n))

(define (f-1-11-iter-impl a b c count)
  (if (= count 0)
	  a
	  (f-1-11-iter-impl b
						c
						(+ c (* b 2) (* a 3))
						(- count 1))))

