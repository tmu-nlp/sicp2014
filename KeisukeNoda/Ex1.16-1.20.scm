;;;; Exercise 1.16 ;;;;

(define (square x)  ;; 二乗計算
  (* x x))

;; a * b^n
;; a * b^0 = 1               (n = 0)
;; a * b^n = a * (b ^ n/2)^2 (n = 2m)   (m = 1,2,,,)
;; a * b^n = a * b * b^(n-1) (n = 2m-1) (m = 1,2,,,)

;; 指数nと底bの他にもう一つの状態変数aを用意し計算のステップ数を少なくする

(define (fast-expt-i a b n)
  (cond ((= n 0) a)
        ((even? n)
          (fast-expt-i a (square b) (/ n 2)))
        (else
          (fast-expt-i (* a b) b (- n 1)))))

;; b^n
;; b^0 = 1           (n = 0)
;; b^n = (b ^ n/2)^2 (n = 2m)     (m = 1,2,...)
;; b^n = b * b^(n-1) (n = 2m - 1) (m = 1,2,...)

(define (fast-expt b n)
  (fast-expt-i 1 b n))

(fast-expt 2 6)

;;;; Exercise 1.17 ;;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))


;; a*b
;; a*b = 0             (b = 0)
;; a*b = 2a * b/2      (b = 2n)     (n = 1,2,...)
;; a*b = a + a * (b-1) (b = 2n - 1) (n = 1,2,...)

(define (multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (multiply (double a) (halve b)))
        (else (+ a (multiply a (- b 1))))))

(multiply 2 6)

;;;; Exercise 1.18 ;;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiply-i n a b)
  (cond ((= b 0) n)
        ((even? b) (multiply-i n (double a) (halve b)))
        (else (multiply-i (+ n a) a (- b 1)))))

(define (multiply2 a b)
  (multiply-i 0 a b))

(multiply2 2 6)

;;;; Exercise 1.19 ;;;;

;; フィボナッチ数計算の対数ステップ版を作成

(define (Fib n)
    (Fib-iter 1 0 0 1 n))

(define (Fib-iter a b p q count)
    (cond
        ((= count 0) b)
        ((even? count)
          (Fib-iter a
		    b
		    (+ (* p p) (* q q))
		    (+ (* 2 p q) (* q q))
		    (/ count 2)))
        (else (Fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


#|----

T:
a ← a + b
b ← a
という変換

T_pq:
a ← bq + aq + ap
b ← bp + aq
という変換

変換 T_pq を2回使うとその効果は同じ形式の変換 T_p'q' を1回使ったのと同じになることを示し、p', q' を、p, q を使って表せ

a = bq + aq + ap
b = bp + aq

T_pq を１回作用させたものを a' b'とする

a' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
   = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
   = a(q^2 + q^2 + pq + pq + p^2) + b(pq + q^2 + pq)
   = a(p^2 + 2q^2 + 2pq) + b(q^2 + 2pq)
   = b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)

b' = (bp + aq)p + (bq + aq + ap)q
   = bp^2 + apq + bq^2 + aq^2 + apq
   = apq + aq^2 + apq + bp^2 + bq^2 
   = a(2pq + q^2) + b(p^2 + q^2)
   = b(p^2 + q^2) + a(q^2 + 2pq)

ここで、
p' = p^2 + q^2
q' = q^2 + 2pq

とすると、
a' = bq' + aq' + ap'
b' = bp' + aq'

となる

----|#

;;;; Exercise 1.20 ;;;;

;; ○正規順序評価
;;   基本的演算子だけを持つ式が出てくるまでパラメタへの被演算子の式の置き換えを繰り返し、
;;   基本的演算子が出現してから初めて評価を行う方式。
;; ○作用的順序評価
;;   引数を評価してから作用させる方式。

;; 作用的順序評価から

(gcd 206 40)

(if (= b 0)
    a
    (gcd b (remainder a b)))

;; １回目
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
;; 条件式が#fなので、代替式が評価
;; (gcd 40 6)

;; ２回目
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))
;; 条件式が#fなので、代替式が評価
;; (gcd 6 4)

;; ３回目
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))
;; 条件式が#fなので、代替式が評価
;; (gcd 4 2)

;;４回目
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
;; 条件式が#fなので、代替式が評価
;; (gcd 2 0)

;; ５回目
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))
;; 条件式が#tなので、帰結式が評価

;; ans 2
;; 合計４回

;; 正規順序評価

;; １回目
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
;; 述語式(= 40 0)が #f になるので、次に評価するのは代替式の
(gcd 40 (remainder 206 40))

;; 2回目
(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; １回目と同じく述語式を評価するとまだ #f
;; この後、しばらく繰り返し #f を返すことが想像できる
