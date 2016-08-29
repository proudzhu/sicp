;; ex 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
						 (if (pair? node)
						     (count-leaves node)
							 1))
					   t)))

(define t1 (list 1 2 (list 3 4) (list 5 (list 6 7))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))
