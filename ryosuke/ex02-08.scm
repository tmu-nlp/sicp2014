(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bounx y))
                 (+ (upper-bound x) (upper-bound x))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (if (< (car x) (cdr x))
    (cdr x)
    (car x)))
(define (lower-bound x)
  (if (< (car x) (cdr x))
    (car x)
    (cdr x)))

; 差が最小になる区間と最大になる区間の２つになる
(define (sub-interval a b)
  (make-interval 
    (- (lower-bound b) (upper-bound a))
    (- (upper-bound b) (lower-bound a))))

; example
(define a (make-interval 1 2))
(define b (make-interval 3 5))

(print (sub-interval a b))


