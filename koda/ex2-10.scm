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
	(cond ((or (<= (upper-bound y) 0)
			   (= (lower-bound y) 0))
		   (print "error"))
		  (else (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y)))))))


(define pair1 (make-interval -3 3)) 
(define pair2 (make-interval 1 3)) 
(print pair1)
(print pair2)
(print (div-interval pair1 pair2)) 

