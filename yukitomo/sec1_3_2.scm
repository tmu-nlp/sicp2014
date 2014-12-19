;sec1_3_2.scm
(define (square x) (* x x)) 
;pi-sum(1.3.1)
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
(pi-sum 1 5000)

;pi-sum(lambda)
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) 
       a
       (lambda (x) (+ x 4)) ;input + 4
       b))

;(pi-sum 1 3)

;integral
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4 x) (+ x 4))
(plus4 10)
(define plus4 (lambda (x) (+ x 4)))
(plus4 10)

((lambda (x y z) (+ x y (square z))) 1 2 3)

;f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
; a = 1 + xy, b = 1 - y
;f(x,y) = xa^2 + yb + ab

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))
(f 4 5)

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(f 4 5)

(define (f x y)
  (let ((a (+ 1 (* x y))) ;define a, b
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 4 5)

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)

(let ((x 3)
      (y (+ x 2)))　
  (* x y))

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))