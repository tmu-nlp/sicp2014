(use srfi-27)
(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0)
		 1)
		((even? exp)
		 (remainder
		   (square
			 (expmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base
			   (expmod base (- exp 1) m))
			m))))
(define (expmod-a base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

(print (expmod-a 2 10 10))

;Alyssaのexpmodはremainderの使用回数が最後の1回である
;そのため引数が大きく指数計算に時間がかかる場合は遅くなってしまう
;

