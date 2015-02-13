#lang scheme

; 2つの区間の積を考えたときに
; 許容誤差の 2乗 が十分に小さいことを利用して近似できることを示す
; 数値が正であると前提して検証

#|

2つの区間 を
A:中央値a, 許容誤差w1
B:中央値b, 許容誤差w2
とすれば,

A = [a * (1 - w1), a * (1 + w1)]
B = [b * (1 - w2), b * (1 + w2)]

となる. A*B の上端(uAB)について考えると
 
uAB = ab + abw1 + abw2 + w1w2
                         \--/
                      とても小さい

uAB ~ ab + abw1 + abw2 + 0
    = ab + (w1 + w2)ab

と簡略化できる. これはA*Bの下端についても同様に成り立つ.
ゆえに, 許容誤差が小さい区間同士の積の許容誤差は
元の2つの区間の許容誤差の和に近似できる.

|#


; 以下 実行テスト

; 近似区間積(数値が正だけの場合)
(define (prox-mul-interval x y)
  (let ((a (center x))
        (w1 (percent x))
        (b (center y))
        (w2 (percent y)))
    (make-center-width (* a b) (+ w1 w2))))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))


(define (make-interval lower upper) (cons lower upper))
(define (lower-bound x) (if (pair? x) (car x) #f))
(define (upper-bound x) (if (pair? x) (cdr x) #f))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (= 0 (* (upper-bound y) (lower-bound y)))
      (error)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))


; run
(define a (make-center-percent 10000 3))
(define b (make-center-percent 30000 2))
(define c (mul-interval a b))
(define d (prox-mul-interval a b))

c
(percent c)
d
(percent d)
