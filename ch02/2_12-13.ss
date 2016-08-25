(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; ex 2.12
(define (make-center-percent c p)
  (make-interval (- c (* c w))
				 (+ c (* c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

;; ex 2.13
;; for positive x([a, b]) y([c, d])
;; x * y = [a, b] * [c, d] = [a * c, b * d]
;; for small width x y, ie. a =~ b, c ~= d
;; percent x + percent y =~ percent (x * y)
