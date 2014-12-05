(define (expmod base exp m)
  (cond ((= exp 0)
		 1)
		((even? exp)
		 (remainder
		   (* (expmod base (/ exp 2) m)
			  (expmod base (/ exp 2) m))
		   m))
		(else
		  (remainder
			(* base
			   (expmod base (- exp 1) m))
			m))))

(print (expmod 53 8 3))
;squareだとexpmodを呼び出すのが一回なのに対し，
;明示的な乗算だとexpmodを呼び出すのが二回なので
;データ量を半減させる効果を相殺してしまうため
