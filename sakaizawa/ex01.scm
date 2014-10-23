--------------------------------ex01---------------------------------------
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

--------------------------------ex02---------------------------------------

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

--------------------------------ex03---------------------------------------

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex03 a b c)
  (if (and (> a b) (> b c)) (sum-of-squares a b))
  (if (and (> a c) (> c b)) (sum-of-squares a c))

  (if (and (> b a) (> a c)) (sum-of-squares b a))
  (if (and (> b c) (> c a)) (sum-of-squares b c))

  (if (and (> c a) (> a b)) (sum-of-squares c a))
  (if (and (> c b) (> b a)) (sum-of-squares c b)))

--------------------------------ex04---------------------------------------

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

--------------------------------ex05---------------------------------------

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

--------------------------------ex06---------------------------------------
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

--------------------------------ex07---------------------------------------
(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (/ (- (square guess) x) (* 2 x))) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

--------------------------------ex08---------------------------------------
(define (cub x) (* x x x))
(define (square x) (* x x))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average (* 2 guess) (/ x (square guess))))

(define (average x y)
  (/ (+ x y) 3))

(define (good-enough? guess x)
  (< (abs (- (cub guess) x)) 0.001))

(define (cube x)
  (cube-iter 1.0 x))



