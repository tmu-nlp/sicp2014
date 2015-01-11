;ex2_08.scm

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;差分の定義
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define r1 (make-interval 6 8))
(define r2 (make-interval 7 9))

(sub-interval r1 r2)
