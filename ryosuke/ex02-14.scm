(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
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



;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define small (make-center-width 5 0.1))
(define large (make-center-width 8 0.1))
(print (par1 small large))
(print (par2 small large))

