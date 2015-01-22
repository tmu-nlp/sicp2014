;exercise 1.43

;前のから引用
(define (square x)
   (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

;---ここから1.43---
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 3) 5)