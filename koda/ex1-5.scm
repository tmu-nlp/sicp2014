(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(print (test 0 (p)))

;(p)を再帰的に読み込むのを繰り返すため，無限ループに入る
