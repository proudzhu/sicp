;; ex 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define x 1)
(define y 2)
(define z (cons x y))

(newline)
(display (car z))
(display ", ")
(display (cdr z))

;; car cdr
;; (car z)
;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x
