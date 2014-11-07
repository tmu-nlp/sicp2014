;Tpq: a <- bq+aq+ap = (p+q)a +qb b <- bp+aq
;Tp'q' a <-  (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p
;           = (2q^2+2pq+p^2)a + (q^2+2pq)b
;           = (p'+q')a+q'b
;      b <-  (bp+aq)p + (bq+aq+ap)
;           = (q^2+2pq)a + (p^2+q^2)b
;           = q'a + p'b
;=> p' = p^2+q^2
;   q' = q^2+2pq

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q));p'を計算
                   (+ (square q) (* 2 p q));q'を計算
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;test
(fib 5)
; = 5
