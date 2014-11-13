(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
(print (+ 4 5))
;展開を繰り返すため再帰である　
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))
(print (+ 4 5))
;展開も収縮もしないため反復である　
