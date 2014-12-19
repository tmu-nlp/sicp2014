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
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
	x
	(make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y)))))


(define pair1 (make-interval 2 3)) 
(define pair2 (make-interval 9 1)) 
(print pair1)
(print pair2)
(print (add-interval pair1 pair2)) 
(print (sub-interval pair1 pair2)) 
(print (mul-interval pair1 pair2)) 
(print (div-interval pair1 pair2)) 

;pair1:(2 . 3) 1
;pair2:(9 . 1) 8
;add:(3 . 12) 9
;sub:(-7 . 2) 9
;mul:(2 . 27) 25 
;div:(0.2222222222222222 . 3.0) 2.777...)
