;; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; one
;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
;; (lambda (f) (lambda (x) (f x)))

;; two
;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (f y)))) x))))
;; (lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
;; (lambda (f) (lambda (x) (f (f x))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
	(lambda (x)
	  ((a f) ((b f) x)))))

