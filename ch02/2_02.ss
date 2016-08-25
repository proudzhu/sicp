;; ex 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
		(end (end-segment seg)))
	(make-point (average (x-point start)
						 (x-point end))
				(average (y-point start)
						 (y-point end)))))

(define (average a b)
  (/ (+ a b) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define one-two
  (make-point 1 2))

(define three-four
  (make-point 3 4))

(define seg1234
  (make-segment one-two three-four))

(print-point (midpoint-segment seg1234))
