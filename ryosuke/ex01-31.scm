; gosh ex01-31.scm

; recursive process
(define (r-product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))


; iterative process
(define (i-product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; factorial
(define (factorial n)
  (define (inc i) (+ i 1))
  (define (buff n) n)
  (i-product buff 1 inc n))

; pi/4
(define (quarter-pi n)
  (define (square x) (* x x))
  (define (inc2 x) (+ 2 x))
  (define (even? x) (= (remainder x 2) 0))
  
  (if (not (even? n)) (set! n (+ n 1)))
  (exact->inexact
   (/ (* 2 (+ n 2) (i-product square 2 inc2 n))
      (i-product square 3 inc2 (+ n 1)))))

; test
(print "(factorial 5)")
(print (factorial 5))
(print "(factorial 6)")
(print (factorial 6))

(print "(quarter-pi 100)")
(print (quarter-pi 100))
(print "(quarter-pi 1000)")
(print (quarter-pi 1000))

