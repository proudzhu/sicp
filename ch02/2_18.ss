;; ex 2.18
(define (reverse items)
  (if (null? (cdr items))
	  items
      (append (reverse (cdr items))
			  (list (car items)))))

