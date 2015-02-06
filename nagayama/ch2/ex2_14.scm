#lang scheme

; 並列接続の計算
; par1,par2 で計算結果が異なることを確認する
; 適当な区間 A,B を用意して
; A/A, A/B の値がどのようになるか調べる

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))


; 以下実行テスト

(define (make-interval lower upper) (cons lower upper))
(define (lower-bound x) (if (pair? x) (car x) #f))
(define (upper-bound x) (if (pair? x) (cdr x) #f))

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
(define a (make-center-percent 2000 10))
(define b (make-center-percent 1000 5))

(define par1ab (par1 a b))
(define par2ab (par2 a b))
(display "par1ab : ") par1ab
(display "par2ab : ") par2ab

(define par1aa (par1 a a))
(define par2aa (par2 a a))
(display "par1aa : ") par1aa
(display "par2aa : ") par2aa

(newline)

(define aa (div-interval a a))
(display "aa : ") aa
(define ab (div-interval a b))
(display "ab : ") ab

(display "center_aa : ") (center aa)
(display "center_ab : ") (center ab)

(display "percent_aa : ") (percent aa)
(display "percent_ab : ") (percent ab)




 


