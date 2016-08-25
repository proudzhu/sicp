;; ex 2.3
(define (perimeter rect)
  (* 2 (+ (width rect)
		  (height rect))))

(define (area rect)
  (* (width rect)
	 (height rect)))

;; from ex 2.2
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

(define (segment-length seg)
  (let ((a (start-segment seg))
		(b (end-segment seg)))
	(sqrt (+ (square (- (x-point b)
						(x-point a)))
			 (square (- (y-point b)
						(y-point a)))))))

(define (square x)
  (* x x))

;; Impl 1, using left-top and right-bottom points
(define (make-rect left-top right-bottom)
  (cons left-top right-bottom))

(define (left-top rect)
  (car rect))

(define (right-bottom rect)
  (cdr rect))

(define (left-bottom rect)
  (make-point (x-point (left-top rect))
			  (y-point (right-bottom rect))))

(define (right-top rect)
  (make-point (x-point (right-bottom rect))
			  (y-point (left-top rect))))

(define (width rect)
  (segment-length (make-segment (left-bottom rect)
								(right-bottom rect))))

(define (height rect)
  (segment-length (make-segment (left-bottom rect)
								(left-top rect))))

(define r1 (make-rect
			 (make-point 4 8)
			 (make-point 20 16)))

(newline)
(display (area r1))
;; 128
(newline)
(display (perimeter r1))
;; 48

;; Impl2
(define (make-rect left-seg bottom-seg)
  (cons left-seg bottom-seg))

(define (left-seg rect)
  (car rect))

(define (bottom-seg rect)
  (cdr rect))

(define (width rect)
  (segment-length (bottom-seg rect)))

(define (height rect)
  (segment-length (left-seg rect)))

(define r2
  (make-rect (make-segment (make-point 4 8)
						   (make-point 4 16))
			 (make-segment (make-point 4 8)
						   (make-point 20 8))))

(newline)
(display (aera r2))
;; 128
(newline)
(display (perimeter r2))
;; 48
