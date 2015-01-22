;exercise 1.44

;1.43から引用
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

;---ここからsmoothing---
(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3))))

(define (n-fold-smooth n f)
  (repeated smooth n) f)

