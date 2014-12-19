(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
	   a
	   (lambda (x) (+ x 4))
	   b))

(define (integral f a b dx)
  (* (sum f
		  (+ a (/ dx 2.0))
		  (lambda (x) (+ x dx))
		  b)
	 dx))
(define (f x y)
  (define (f-helper a b)
	(+ (* x (square a))
	   (* y b)
	   (* a b)))
  (f-helper (+ 1 (* x y))
			(- 1 y)))

(define (f g) (g 2))
(print (f square))
(print (f (lambda (z) (* z (+ z 1)))))
(print (f f))
(print (f 2))
(print (2 2))
;最終的に2で2を評価するという形になるが,2という手続きはないためエラーが返ってくる
