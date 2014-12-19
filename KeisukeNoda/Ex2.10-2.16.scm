;;;; Exercise 2.10 ;;;;


;; 0をまたがる区間で割った時、どうなるか分からないことを調べる
(define R1 (make-interval 5 10))
(define R2 (make-interval 1 5))
(define R3 (make-interval -10 -5))
(define R4 (make-interval -5 10))

;; 2.1.4で定義した演算
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
;; 区間構成子
(define (make-interval a b) (cons a b))

(div-interval R1 R2)
;; gosh> (1.0 . 10.0)
;; good job!

(div-interval R1 R3)
;; gosh> (-2.0 . -0.5)
;; good job!!!

(div-interval R1 R4)
;; gosh> (-2.0 . 1.0)
;; 期待通りではない.
;; ほんとは(-1, 1)になってほしい.

;; div-intervalはmul-intervalを利用していて
;; mul-intervalで全ての区間の組み合わせで
;; 乗算を行ったあとに、大小関係を判定しているため.
;; 
(define (new-div-interval x y)
  (let ((ly (lower-bound y))
        (uy (upper-bound y)))
    (if (< (* ly uy) 0)
        (error "error")
        (mul-interval x
                      (make-interval (/ 1.0 ly)
                                     (/ 1.0 uy))))))

(new-div-interval R1 R2)
;; gosh> (1.0 . 10.0)
(new-div-interval R1 R3)
;; gosh> (-2.0 . -0.5)
(new-div-interval R1 R4)
;; gosh> *** ERROR: error
;; Stack Trace:
;; _______________________________________
;;   0  (eval expr env)
;;         At line 179 of "/usr/local/share/gauche-0.9/0.9.4/lib/gauche/interactive.scm"


;;;; Exercise 2.11 ;;;;

;; 区間の端点の符号を考えるので、区間と零との比較を考える.

;; ９つの場合分けは
;; 1. ux > 0, lx > 0 かつ uy > 0, ly > 0
;; 2. ux > 0, lx > 0 かつ uy > 0, ly < 0
;; 3. ux > 0, lx > 0 かつ uy < 0, ly < 0
;; 4. ux > 0, lx < 0 かつ uy > 0, ly > 0
;; 5. ux > 0, lx < 0 かつ uy > 0, ly < 0
;; 6. ux > 0, lx < 0 かつ uy < 0, ly < 0
;; 7. ux < 0, lx < 0 かつ uy > 0, ly > 0
;; 8. ux < 0, lx < 0 かつ uy > 0, ly < 0
;; 9. ux < 0, lx < 0 かつ uy < 0, ly < 0

;; 2回を超える乗算が必要なのは2の時？

(define (new-mul-interval x y)
  (let ((ux (upper-bound x))
        (lx (lower-bound x))
        (uy (upper-bound y))
        (ly (lower-bound y)))
    (cond [(and (> ux 0) (> lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* ux uy)
                                 (* lx ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (* ux uy)
                                 (* lx ly))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* ux ly)
                                 (* lx uy))])]
          [(and (> ux 0) (< lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* ux uy)
                                 (* lx ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (min (* ux ly) (* lx uy))
                                 (max (* lx ly) (* ux uy)))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* lx ly)
                                 (* ux uy))])]
          [(and (< ux 0) (< lx 0))
           (cond [(and (> uy 0) (> ly 0))
                  (make-interval (* lx uy)
                                 (* ux ly))]
                 [(and (> uy 0) (< ly 0))
                  (make-interval (* lx ly)
                                 (* ux uy))]
                 [(and (< uy 0) (< ly 0))
                  (make-interval (* ux uy)     


;;;; Exercise 2.12 ;;;;


;; 区間構成子
(define (make-interval a b) (cons a b))

;; 選択子upper-bound, lower-boundの実装
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; 中央値と許容誤差で表す数を扱う構成子と選択子
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


;; 中央値とパーセント許容誤差をとり、区間を返す構成子
(define (make-center-percent c p)
  (let ((per (/ p 100.0)))
    (make-interval (- c (* c per))
                   (+ c (* c per)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((width (/ (- (upper-bound i) (lower-bound i)) 2)))
    (* (/ width (center i)) 100)))

;; 10パーセントの許容誤差で6.8オーム
(define R1 (make-center-percent 6.8 10))
(display R1)
;; gosh> (6.12 . 7.4799999999999995)

(center R1)
;; gosh> 6.8

(percent R1)
;; gosh> 9.999999999999996 


;;;; Exercise 2.13 ;;;;

;;            中央値      = center
;;            許容誤差    = width
;; (パーセント)相対許容誤差 = percent
;; percent を P とすると、
;; やりたいことはP(x × y) ≒ f(P(x), P(y))の形式で表現できることを示せ
;; というお話？

;; 2つの区間の積の相対許容誤差は，全区間が正の範囲内と仮定すると，
;; P(A × B) = { W(A × B) / C(A × B) } × 100
;; ここで，
;; A × B    = [{C(A) - W(A)} × {C(B) - W(B)}, {C(A) + W(A)} × {C(B) + W(B)}]
;; W(A × B) = 1/2 * [{C(A) + W(A)} × {C(B) + W(B)} - {C(A) - W(A)} × {C(B) - W(B)}]
;;          = C(A)・ W(B) + C(B)・W(A)
;; C(A × B) = 1/2 * [{C(A) - W(A)} × {C(B) - W(B)} + {C(A) + W(A)} × {C(B) + W(B)}]
;;          = C(A)・C(B) + W(A)・W(B)
;;
;; ここで，相対許容誤差は中央値より十分小さいという仮定から，
;; C(A × B) ≒ C(A)・C(B)
;;
;; これらを最初の式に代入
;; P(A × B) ≒ [{C(A)・W(B) + C(B)・W(A)\} / {C(A)・C(B)}] × 100
;;          = {W(B) / C(B) + W(A) / C(A)} × 100
;;          = P(A) + P(B)


;;;; Exercise 2.14 ;;;;

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1 )))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; par1とpar2で結果が異なることの確認
(define R1 (make-center-percent 6.8 10))
(define R2 (make-center-percent 4.7 5))
(display R1)
(display R2)
(par1 R1 R2)
;; gosh> (2.201031010873943 . 3.4873689182805854)
(par2 R1 R2)
;; gosh> (2.581558809636278 . 2.97332259363673)

;; 幅が中央値に比べて小さいパーセントの区間を用いて確認
(define R3 (make-center-percent 10 1))
(display R3)
(define R4 (make-center-percent 20 1))
(display R4)

(define div-R3 (div-interval R3 R3))
(center div-R3)
;; gosh> 1.0002000200020003
(percent div-R3)
;; gosh> 1.9998000199979908
;; 誤差が1%から約2%へと増加している.

(define div-R3-R4 (div-interval R4 R3))
(center div-R3-R4)
;; gosh> 2.0004000400040005
(percent div-R3-R4)
;; gosh> 1.9998000199979908
;; やはり誤差が増加している.

;; 区間同士の乗算及び除算を行うと誤差が 
;; 増加していくことが推察できる.

;;;; Exercise 2.15 ;;;;


(define R3 (make-center-percent 10 1))
(define R4 (make-center-percent 20 1))
(define R5 (make-center-percent 10 3))
(define R6 (make-center-percent 10 0.1))
(define R7 (make-center-percent 1000 50))

(define (check-percent x y)
  (percent (div-interval x y)))

;; 引き続いて振る舞いについての観察

;; 同一区間での除算の時の誤差
(check-percent R3 R3)
;; gosh> 1.9998000199979908

;; 誤差が同じで中央値が異なる時の誤差
(check-percent R4 R3)
;; gosh> 1.9998000199979908

;; 誤差の大きさが異なる時の誤差
(check-percent R5 R3)
;; gosh> 3.998800359892036

;; 誤差が大きく異なるときの誤差
(check-percent R7 R6)
;; gosh> 50.07496251874062

;; 他の場合と比較して単純な誤差の和になっていない.
;; 誤差の大きさが大きく異なる場合、
;; 小さい方の誤差の影響はほとんど無視出来るほど小さくなることが予想される.


;; 以上から、par1よりpar2のほうが「より良い」プログラムであるというのは真.
;; par2は双方ともに区間を用いるのではなく、 
;; 片方の区間を定数(誤差0)にすることで相対誤差の増加を抑えているため.


;;;; Exercise 2.16 ;;;;


;; 代数演算的に R1R2 / (R1+R2) の分母と分子が同一のR1とは言えないようになってしまっているから
;; 代数的なアプローチは適当にWebを検索したら出てくる.

;; 代数的なアプローチではない場合
;; 構文解析をして、抵抗を計算するものは強制的に誤差の伝播が少ない形式にする.
;; 代数変形するいくつかのパターンをあらかじめ用意しておいて、誤差の伝播が少ないパターンで計算するとか.
