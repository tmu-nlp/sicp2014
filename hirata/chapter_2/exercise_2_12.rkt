(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (cdr x))
(define (lower-bound x)
  (car x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* (abs c) p) 100)))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let* ((w (/ (- (upper-bound i) (lower-bound i)) 2))
	 (c (center i)))
    (* (/ w c) 100)))

(define small (make-interval 2 4))
(define big (make-interval -1 0))