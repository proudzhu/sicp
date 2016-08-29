;; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
	   m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
	(map (lambda (m-row)
		   (map (lambda (n-col)
				  (dot-product m-row n-col))
				n-cols))
		 m)))


(define m1 (list (list 1 2) (list 3 4) (list 5 6)))
(define m2 (list (list 1 1) (list 1 2)))
(define v (list 1 2))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  nil
	  (cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs)))))

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))
