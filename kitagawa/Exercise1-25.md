Exercise 1.25
=====================

最初の expmod は以下のような定義である.

	(define (expmod base exp m)
     (cond ((= exp 0) 1)
           ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                       m))
           (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))

Alyssa の提案した expmod は以下のような定義である.

	(define (expmod base exp m)
     (remainder (fast-expt base exp) m))

   (define (fast-expt b n)
     (cond ((= n 0) 1)
           ((even? n) (square (fast-expt b (/ n 2))))
           (else (* b (fast-expt b (- n 1))))))

これは高速素数テストと同じようには使えない.
なぜなら remainder に使う引数が巨大数になるため演算に時間がかかるためである.