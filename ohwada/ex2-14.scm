; make-interval

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))


; make-center-width, center, width

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; make-center-persent, persent

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))


;(print (center (make-center-percent 40 20)))
;(print (percent (make-center-percent 40 20)))
;(print (make-center-percent 40 20)) ; 40 * 0.2 = 8 なので (32, 48)



; div-interval

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))



; par1 と par2

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))






; 区間 A と B を定義する

(define A (make-center-percent 5 1))
(define B (make-center-percent 9 2))



; テスト
; A と B の par1 と par2 を計算

(print (center (par1 A B)))
(print (center (par2 A B)))
(print (percent (par1 A B)))
(print (percent (par2 A B)))

; A/A と A/B を計算

(print (center (div-interval A A)))
(print (center (div-interval A B)))
(print (percent (div-interval A A)))
(print (percent (div-interval A B)))
