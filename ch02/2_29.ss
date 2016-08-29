;; ex 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define mobile1 (make-mobile 1 2))
(left-branch mobile1)
(right-branch mobile1)

(define branch1 (make-branch 1 2))
(branch-length branch1)
(branch-structure branch1)

;; b
(define (structure-is-mobile? structure)
  (pair? structure))

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
	(if (structure-is-mobile? s)
	    (total-weight s)
		s)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
	 (branch-weight (right-branch mobile))))

;; c
(define (branch-balanced? branch)
  (let ((s (branch-structure branch)))
	(if (structure-is-mobile? s)
	    (balanced? s)
		true)))

(define (branch-torque branch)
  (* (branch-weight branch)
	 (branch-length branch)))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
		(right (right-branch mobile)))
	(and (= (branch-torque left)
			(branch-torque right))
		 (branch-balanced? left)
		 (branch-balanced? right))))

;; d
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (structure-is-mobile? structure)
  (pair? structure))
