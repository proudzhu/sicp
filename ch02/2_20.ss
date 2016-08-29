;; ex 2.20
(define (same-parity x . xs)
  (let ((parity (even? x)))
	(define (iter result items)
	  (cond ((null? items) result)
			((equal? (even? (car items)) parity)
			 (iter (append result
						   (list (car items)))
				   (cdr items)))
			(else
			  (iter result (cdr items)))))
	(iter (list x) xs)))
