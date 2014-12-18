(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bounx y))
                 (+ (upper-bound x) (upper-bound x))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (if (< (car x) (cdr x))
    (cdr x)
    (car x)))
(define (lower-bound x)
  (if (< (car x) (cdr x))
    (car x)
    (cdr x)))

(define (sub-interval a b)
  (make-interval 
    (- (lower-bound a) (upper-bound b))
    (- (upper-bound a) (lower-bound b))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
    (error "error")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))


;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;
(define (make-center-percent c p)
  (let ((w (/ (* (abs c) p) 100)))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let* ((w (/ (- (upper-bound i) (lower-bound i)) 2))
         (c (center i)))
    (* (/ w c) 100)))


; example
(define test (make-center-percent 90 5))
(print test)
(print (percent test))

