(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2.0))
(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(print (((double (double double)) inc) 5))

(((double (lambda (x) (double (double x)))) inc) 5)
(((lambda (x) (double (double (double (double x))))) inc) 5)
;doubleが四回入れ子になるため2^4で16回インクリメントされ21になる
