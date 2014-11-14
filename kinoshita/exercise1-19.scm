;T_pq(a,b) -> a' = bq+aq+ap, b' = bp+aq
;T_pq(a',b') -> a'' = (bp+aq)q+(bq+aq+ap)q+(bq+aq+ap)p
;                   = 2bpq+bq^2 + 2aq^2+2apq+ap^2
;                   = b(2pq+q^2) + a(2pq+q^2) + a(p^2+q^2)
;               b'' = (bp+aq)p+(bq+aq+ap)q
;                   = b(p^2+q^2) + a(2pq+q^2)
;p' = p^2 + q^2
;q' = 2pq + q^2
(define (fib n)
    (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
    (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                    b
                    (+ (square p) (square q))
                    (+ (* 2 p q) (square q))
                    (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
#?=(fib 1)
#?=(fib 2)
#?=(fib 3)
#?=(fib 4)
#?=(fib 5)
#?=(fib 6)
