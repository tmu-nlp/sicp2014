; compose
(define (compose f g)
  (lambda (x) (f (g x))))

; 関数 f を n 回作用させる手続き repeated
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (square x) (* x x))
(define (cube x) (* x x x))

(print ((repeated square 2) 5)) ; (5*5)*(5*5) = 625
(print ((repeated cube 2) 2)) ; (2*2*2)*(2*2*2)*(2*2*2) = 512 
