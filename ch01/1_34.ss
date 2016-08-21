;; ex 1.34
(define (f g)
  (g 2))

(define (square x) (* x x))

(f square)
;; 4

(f (lambda (z) (* z (+ z 1))))
;; 6
