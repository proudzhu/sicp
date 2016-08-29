;; ex 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		     (tree-map proc sub-tree)
			 (proc sub-tree)))
	   tree))

(define (square-tree tree)
  (tree-map square tree))

(define nil '())

(define (square x) (* x x))

(define t1
  (list 1
		(list 2 (list 3 4) 5)
		(list 6 7)))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
		     (square-tree2 sub-tree)
			 (square sub-tree)))
	   tree))
