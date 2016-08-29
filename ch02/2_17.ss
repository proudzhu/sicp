;; ex 2.17
(define (last-pair items)
  (define (iter a l)
	(if (null? l)
	    a
		(iter (car l) (cdr l))))
  (iter items items))
