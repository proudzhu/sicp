;; ex 2.41
(define (ordered-triples-sum n s)
  (filter (lambda (l) (= (accumulate + 0 l) s))
		  (flatmap
			(lambda (i)
			  (flatmap
				(lambda (j)
				  (map (lambda (k) (list i j k))
					   (enumerate-interval 1 (- j 1))))
				(enumerate-interval 1 (- i 1))))
			(enumerate-interval 1 n))))

(define (enumerate-interval low high)
  (if (> low high)
	  nil
	  (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

