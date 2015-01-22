; 合成関数を表す手続き compose
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))


(print ((compose square inc) 6)) ; (6+1)*7 = 49
(print ((compose inc square) 6)) ; (6*6)+1 = 37
