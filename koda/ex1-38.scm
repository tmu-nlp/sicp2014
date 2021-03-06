(use srfi-27)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (average x y)
  (/ (+ x y) 2.0))

;再帰
(define (cont-frac n d k)
  (define (cont-frac-iter i)
	(cond ((< i k)
		   (/ (n i) (+ (d i) (cont-frac-iter (+ i 1)))))
		  ((= i k)
		   (/ (n i) (d i)))))
  (cont-frac-iter 1))
;線形
(define (cont-frac-int n d k)
  (define (cont-frac-iter i result)
	(if (= i 0)
	  result
	  (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

(define (di i)
  (cond ((= i 2) 2)
		((= 0 (remainder (- i 2) 3)) (* 2 (+ (/ (- i 2) 3) 1)))
		(else 1)))

;ネイピア数
(print 2.71828)
(print (+ 2.0 (cont-frac (lambda (i) 1.0) di 10)))

