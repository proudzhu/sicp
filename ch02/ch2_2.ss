;; list operations
(define (list-ref items n)
  (if (= n 0)
	  (car items)
	  (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
	  0
	  (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)

(define (length items)
  (define (length-iter a count)
	(if (null? a)
	    count
		(length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
	  list2
	  (cons (car list1) (append (cdr list1) list2))))

;; hierarchical structures
(define (count-leaves l)
  (cond ((null? l) 0)
		((not (pair? l)) 1)
		(else (+ (count-leaves (car l))
				 (count-leaves (cdr l))))))

(define x (cons (list 1 2) (list 3 4)))

;; nested mapping
(define (permutations s)
  (if (null? s)
	  (list nil)
	  (flatmap (lambda (x)
				 (map (lambda (p) (cons x p))
					  (permutations (remove x s))))
			   s)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
		  sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

