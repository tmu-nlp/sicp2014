;ex2_12.scm
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;区間オブジェクトから中央値、区間の大きさを取得
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;中央値、パーセント相対許容誤差を取りオブジェクトを定義
(define (make-center-percent c p)
  (make-interval (- c (* (/ c 100.0) p))
                 (+ c (* (/ c 100.0) p))))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define i (make-center-percent 200 5))
(percent i)
(center i)
(width i)