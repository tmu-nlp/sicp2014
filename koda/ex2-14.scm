(use srfi-27)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (if (< (car x) (cdr x))
						  (car x)
						  (cdr x)))
(define (upper-bound x) (if (> (car x) (cdr x))
						  (car x)
						  (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
		(p2 (- (lower-bound x) (upper-bound y)))
		(p3 (- (upper-bound x) (lower-bound y)))
		(p4 (- (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((lower-x (lower-bound x))
		(lower-y (lower-bound y))
		(upper-x (upper-bound x))
		(upper-y (upper-bound y)))
	(cond ((> lower-x 0)
		   (cond ((> lower-y 0)
				  (make-interval (* lower-x lower-y) (* upper-x upper-y)))
				 ((< upper-y 0)
				  (make-interval (* upper-x lower-y) (* lower-x upper-y)))
				 (else
				   (make-interval (* upper-x lower-y) (* upper-x upper-y)))))
		  ((< upper-x 0) 
		   (cond ((> lower-y 0)
				  (make-interval (* lower-x upper-y) (* upper-x lower-y)))
				 ((< upper-y 0)
				  (make-interval (* upper-x upper-y) (* lower-x lower-y)))
				 (else
				   (make-interval (* lower-x lower-y) (* lower-x upper-y)))))
		  (else 
		   (cond ((> lower-y 0)
				  (make-interval (* lower-x upper-y) (* upper-x upper-y)))
				 ((< upper-y 0)
				  (make-interval (* upper-x lower-y) (* lower-x lower-y)))
				 (else
				   (make-interval (min (* lower-x upper-y) (* upper-x lower-y))
								  (max (* lower-x lower-y) (* upper-x upper-y)))))))))

 
(define (div-interval x y)
  (mul-interval
	x
	(make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (newline)
  (display (center i))
  (display "±")
  (display (/ (* (width i) 100.) (center i)))
  (display "%")
  (newline)) 

(define (percentage a b)
  (/ (* a 100.0) b))

(define (mul-percent x y)
  (let ((per-x (/ (width x) (center x)))
		(per-y (/ (width y) (center y)))
		(cen-x (center x))
		(cen-y (center y)))
	(newline)
	(display (* cen-x cen-y))
	(display "±")
	(display (* (+ per-x per-y) 100. ))
	(display "%")
	(newline))) 

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
				(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
	(div-interval
	  one (add-interval (div-interval one r1)
						(div-interval one r2)))))

(define a (make-center-width 1000. 5))
(define b (make-center-width 1500. 5))
(define r1 (div-interval a a))
(define r2 (div-interval a b))

(percent a) 
(percent b) 

(percent (par1 r1 r2)) 
(percent (par2 r1 r2)) 

