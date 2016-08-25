;; ex 2.7
(define (make-interval lower-bound upper-bound)
  (cons lower-bound upper-bound))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
				 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

;; ex 1.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
		   (>= (upper-bound y) 0))
	(error "divide by interval which spans zero!")
	(mul-interval x
				  (make-interval (/ 1.0 (upper-bound y))
								 (/ 1.0 (lower-bound y))))))

;; ex 2.9
(define (width-interval interval)
  (/ (- (upper-bound interval)
		(lower-bound interval))
	 2))

;; [a, b] + [c, d] = [a + c, b + d]
;; width: (b - a) / 2 + (d - c) / 2 = ((b + d) - (a + c)) / 2
;; [a, b] + [c, d] = [a - d, b - c]
;; sub: (b - a) / 2 + (d - c) / 2 = ((b - c) - (a - d)) / 2
;; mul and div is not certain

(define (print-interval interval)
  (newline)
  (display "[")
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]"))

(define x (make-interval 1 3))
(define y (make-interval 2 8))
(print-interval (add-interval x y)) ;; [3, 11]
(print-interval (sub-interval x y)) ;; [-7, 1]
(print-interval (mul-interval x y)) ;; [2, 24]
(print-interval (div-interval x y)) ;; [1/8, 3/2]

(define z (make-interval -1 1))
;; (div-interval x z)

;; ex 2.11
(define (mul2-interval x y)
  (let ((xl (lower-bound x))
		(xu (upper-bound x))
		(yl (lower-bound y))
		(yu (upper-bound y)))
	(cond ((and (<= xu 0) (<= yu 0))
		   (make-interval (* xu yu) (* xl yl)))
		  ((and (<= xu 0) (< yl 0) (> yu 0))
		   (make-interval (* xl yu) (* xl yl)))
		  ((and (<= xu 0) (>= yl 0))
		   (make-interval (* xl yu) (* xu yl)))
		  ((and (< xl 0) (> xu 0) (<= yu 0))
		   (make-interval (* xu yl) (* xl yl)))
		  ((and (< xl 0) (> xu 0) (< yl 0) (> yu 0))
		   (make-interval (min (* xl yu) (* xu yl))
						  (max (* xl yl) (* xu yu))))
		  ((and (< xl 0) (> xu 0) (>= yl 0))
		   (make-interval (* xl yu) (* xu yu)))
		  ((and (>= xl 0) (<= yu 0))
		   (make-interval (* xu yl) (* xl yu)))
		  ((and (>= xl 0) (< yl 0) (> yu 0))
		   (make-interval (* xu yl) (* xu yu)))
		  (else ;; (>= xl 0) and (>= yl 0)
			(make-interval (* xl yl) (* xu yu))))))

(print-interval (mul2-interval (make-interval -3 -1)
							   (make-interval -3 -1)))
(print-interval (mul2-interval (make-interval -3 -1)
							   (make-interval -1 1)))
(print-interval (mul2-interval (make-interval -3 -1)
							   (make-interval 1 3)))
(print-interval (mul2-interval (make-interval -1 1)
							   (make-interval -3 -1)))
(print-interval (mul2-interval (make-interval -1 1)
							   (make-interval -1 1)))
(print-interval (mul2-interval (make-interval -1 1)
							   (make-interval 1 3)))
(print-interval (mul2-interval (make-interval 1 3)
							   (make-interval -3 -1)))
(print-interval (mul2-interval (make-interval 1 3)
							   (make-interval -1 1)))
(print-interval (mul2-interval (make-interval 1 3)
							   (make-interval 1 3)))
