;;;; Exercise 2.01 ;;;;


;; 有理数を既約にまで簡約化 ;;

;; 有理数の足し算
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;; 有理数の引き算
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;; 有理数の掛け算
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

;; 有理数の割り算
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

;; 有理数の等価性テスト
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; 有理数の表現
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; 有理数が正なら分子、分母ともに正.
;; 有理数が負なら分子だけを負.
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
     (if (< d 0)
         (cons (/ (- n) g) (/ (- d) g))
         (cons (/ n g) (/ d g)))))

(define (check n d)
  (print-rat (make-rat n d)))

(check 6 9)
;; 2/3

(check 6 -9)
;; -2/3

(check -6 9)
;; -2/3

(check -6 -9)
;; 2/3


;;;; Exercise 2.02 ;;;;

;; 平面上のの線分を表現する             ;;
;; 線分は、始発点と終着点で表現されている ;;

;; 線を表現する
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;; 点を表現する
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; 引数として線分をとり、中間点を返す手続き
(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (print-point (make-point (/ (+ (x-point start) (x-point end)) 2.0)
                       (/ (+ (y-point start) (y-point end)) 2.0)))))

;; 点を出力する手続き
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

 (make-segment (make-point 12 5) (make-point 3 10))

;; gosh> ((12 . 5) 3 . 10)


;;;; Exercise 2.03 ;;;;

; 平面上の長方形の表現を実装する

;; 長方形の表現(左上の点と右下の点)
(define (make-rectangle upper-left-point lower-right-point)
  (cons upper-left-point lower-right-point))

(define (upper-left-point rec)
  (car rec))

(define (lower-right-point rec)
  (cdr rec))

;; 点の表現する
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (calc-width rec)
  (abs (- (x-point (lower-right-point rec))
          (x-point (upper-left-point rec)))))

(define (calc-height rec)
  (abs (- (y-point (upper-left-point rec))
          (y-point (lower-right-point rec)))))

;; 長方形の周囲の長さ
(define (calc-perimeter rec)
  (+ (* 2 (calc-width rec))
     (* 2 (calc-height rec))))

(calc-perimeter (make-rectangle (make-point 5 10) (make-point 10 5)))
;; gosh> 20

;; 長方形の面積
(define (calc-area rec)
  (* (calc-width rec) (calc-height rec)))

(calc-area (make-rectangle (make-point 5 10) (make-point 10 5)))
;; gosh> 25


;; 長方形の違う表現(高さと幅)
(define (make-rectangle height width)
  (cons height width))

(define (rec-height rec)
  (car rec))

(define (rec-width rec)
  (car rec))

(calc-perimeter (make-rectangle (make-point 5 10) (make-point 10 5)))
;; gosh> 20

(calc-area (make-rectangle (make-point 5 10) (make-point 10 5)))
;; gosh> 25 ;; 異なる長方形の表現でも動いている

;;;; Exercise 2.04 ;;;;

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))
;; gosh> 1

(cdr (cons 1 2))
;; gosh> 2

;; (car (cons 1 2))
;; => (car (lambda (m) 1 2))
;; => ((lambda (m) 1 2) (lambda (p q) q))
;; cons手続きで値が２つ渡されており、1つの引数をとるlambdaを返す.
;; carおよび、cdr手続きは、引数にとった手続きに対して、
;; ２つの引数のうちに最初(or 後)の引数を返す手続きを渡す.


;;;; Exercise 2.05 ;;;;

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; carは対で2が割り切れなくなるまで割る回数
(define (car z)
  (define (iter x count)
    (if (> (abs (remainder x 2)) 0)
        count
        (iter (/ x 2) (+ count 1))))
  (iter z 0))

;; cdrは対で割り切れなくなるまで割る回数
(define (cdr z)
  (define (iter x count)
    (if (> (abs (remainder x 3)) 0)
        count
        (iter (/ x 3) (+ count 1))))
  (iter z 0))

;; やってること自体は同じなので更に抽象化できる.
(define (pair z n)
  (define (iter x count)
    (if (> (abs (remainder x n)) 0)
        count
        (iter (/ x n) (+ count 1))))
  (iter z 0))

(define (car z)
  (pair z 2))

(define (cdr z)
  (pair z 3))

(cons 6 10)
;; gosh> 3779136

(car (cons 6 10))
;; gosh> 6

(cdr (cons 6 10))
 ;; gosh> 10


;;;; Exercise 2.06 ;;;;

;; zeroとadd-1を使わずにoneとtwoを実装
;; add-1を繰り返さずに+を定義する

;; 0の定義
(define zero (lambda (f) (lambda (x) x)))
;; 引数として手続きfをとり、
;; 引数としてxをとりxを返す手続きを返す.

;; 1をたす演算
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
;; 引数として手続きnをとる.

;; ある数は、ある関数fを何回xに適用するか、という定義に.
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

;; 1を加えるという処理を複数回繰り返して0,1を実現.
;; 数値として見えるようにincrement処理をする引数を与えてみる.
((one (lambda(x) (+ x 1))) 0)
((two (lambda(x) (+ x 1))) 0)


;; 足し算
;; fをb回適用した後に、fをa回適用する.
;; fをb回適用した値に、fをa回適用することで実現.
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(((add one two) (lambda (x) (+ x 1))) 0)
;; gosh> 3

(((add one one) (lambda (x) (+ x 1))) 0)
;; gosh> 2

(((add two two) (lambda (x) (+ x 1))) 0)
;; gosh> 4


;; べき乗
;; bにaを
(define (exp a b)
  (b a))

(((exp two three) (lambda (x) (+ x 1))) 0)
;; 2**3
;; gosh> 8

(((exp three three) (lambda (x) (+ x 1))) 0)
;; 3**3
;; gosh> 27


;; 掛け算
;; fをb回適用させた手続きにa回適用させる.
;; ↑でb*aを実現.
(define (mul a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))

(((mul two two) (lambda (x) (+ x 1))) 0)
;; 2 * 2
;; gosh> 4

(((mul two three) (lambda (x) (+ x 1))) 0)
;; 2 * 3
;; gosh> 6


