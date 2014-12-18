; a

; a から b まで範囲でのの関数値の積を返す product を定義
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


; product を用いる factorial
(define (factorial a)
  (product identity 1 inc a))

(define (inc x) (+ x 1))
(define (identity x) x)


(print (factorial 5)) ; 120



; 円周率の近似値を計算する手続き pi
(define (pi n)
  (define (next x) (+ x 2))
  (define (term x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
    (* (product term 2 next n)
       4.0))

(print (pi 3000)) ; 3.142...




; b

; 上の product は再帰的
; 以下は反復的な product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(print (pi 3000)) ; 若干速い

