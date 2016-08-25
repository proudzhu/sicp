;; ex 2.5
(define (cons a b)
  (* (expt 2 a)
	 (expt 3 b)))

(define (log-reduce n base)
  (if (not (zero? (remainder n base)))
	  0
	  (+ (log-reduce (/ n base) base) 1)))

(define (car z)
  (log-reduce z 2))

(define (cdr z)
  (log-reduce z 3))

(define z (cons 11 7))
(newline)
(display (car z))
(display ", ")
(display (cdr z))
;; 11, 7
