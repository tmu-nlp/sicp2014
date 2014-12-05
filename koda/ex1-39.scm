(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2.0))

(define (power x n)
  (cond ((= n 1) x)
    (else (* x (power x (- n 1))))))

;再帰
(define (cont-frac n d k)
  (define (cont-frac-iter i)
	(cond ((< i k)
		   (/ (n i) (- (d i) (cont-frac-iter (+ i 1)))))
		  ((= i k)
		   (/ (n i) (d i)))))
  (cont-frac-iter 1))
;線形
(define (cont-frac-int n d k)
  (define (cont-frac-iter i result)
	(if (= i 0)
	  result
	  (cont-frac-iter (- i 1) (/ (n i) (- (d i) result)))))
  (cont-frac-iter k 0))

(define (tan-cf x k)
  (define (ni i)
	(if (= i 1)
	  x
	  (square x)))
  (define (di i)
	(- (* i 2) 1))
  (cont-frac ni di k))

(define pi 3.1415926535)
(print (tan-cf (/ pi 4) 10))
(print (tan-cf (/ pi 2) 10))
(print (tan-cf pi 10))

