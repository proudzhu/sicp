;; ex 2.27
(define (reverse items)
  (if (null? (cdr items))
	  items
      (append (reverse (cdr items))
			  (list (car items)))))

(define (deep-reverse x)
  (if (pair? x)
	  (reverse (map deep-reverse x))
	  x))
