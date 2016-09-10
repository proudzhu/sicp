;; ex 2.60
(define (element-of-set? x set)
  (cons ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
		 (cons (car set1)
			   (intersection-set (cdr set1) (remove-set-element (car set1) set2))))
		(else (intersection-set (cdr set1) set2))))

(define (remove-set-element x set)
  (define (remove-set-element-iter acc rest)
	(cond ((null? rest) acc)
		  ((equal? x (car rest)) (append acc (cdr rest)))
		  (else (remove-set-element-iter (adjoin-set (car rest) acc)
										 (cdr rest)))))
  (remove-set-element-iter '() set))

(define (union-set set1 set2)
  (append set2 set1))
