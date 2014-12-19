; 再帰的な accumulate 手続き
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

; accumulate を用いる sum と product を定義
(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (accumulate combiner 0 term a next b))

(define (product term a next b)
  (define (combiner x y) (* x y))
  (accumulate combiner 1 term a next b))


; テスト
(define (double x) (* x 2))
(define (inc x) (+ x 1))

(print (sum double 1 inc 10))  ; 110
(print (product double 1 inc 3))  ; 2*4*6=48
(print (sum double 1 inc 10000000))


; 反復的
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


(print (sum double 1 inc 10000000)) ; こちらの方が速い
