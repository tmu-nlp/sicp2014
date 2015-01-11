(define dx 0.00001)

; 関数の平滑化を行う手続き smooth
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))


; repeated
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x) (f (g x))))


; repeated を用いた n 重平滑化関数を作る手続き
(define (n-smooth f n)
  ((repeated smooth n) f))


(print ((n-smooth sqrt 3) 3))
