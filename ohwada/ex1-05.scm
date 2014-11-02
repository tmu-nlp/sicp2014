(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(print (test 0 (p)))

; normal-order だと(p)を評価しようとしてループに入る
; applicative-order だと値が必要になるまで評価しない
; ので、x = 0 より 0 を返す。
