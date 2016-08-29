;; ex 2.21
(define (square-list items)
  (if (null? items)
	  nil
	  (cons (square (car items))
			(square-list (cdr items)))))

(define (square x)
  (* x x))

(define nil '())

(define (square-list2 items)
  (map square items))
