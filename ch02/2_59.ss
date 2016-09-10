;; ex 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2)
		 (union-set (cdr set1) set2))
		(else (cons (car set1)
					(union-set (cdr set1) set2)))))

(define (equal? list1 list2)
  (cond ((and (not (pair? list1))
			  (not (pair? list2)))
		 (eq? list1 list2))
		((and (pair? list1)
			  (pair? list2))
		 (and (equal? (car list1) (car list2))
			  (equal? (cdr list1) (cdr list2))))
		(else #f)))

(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

