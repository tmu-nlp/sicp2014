;ex1_19.scm
;a_1 = bq + aq + ap
;b_1 = bp + aq
;a_2 = (bp+aq)q+(bq+aq+ap)q+(bq+aq+ap)p =  a(p^2+2q^2+2pq) + b(q^2+2pq)
;b_2 = (bp+aq)p+(bq+aq+ap)q = a(q^2+2pq) + b(p^2+q^2)

(define (fib n)
  (fib-itr 1 0 0 1 n))

(define (fib-itr a b p q count)
  (define (even? x) (= (remainder x 2) 0))
  (define (square x) (* x x))
  (cond ((= count 0) b)
        ((even? count)
         (fib-itr a b
                  (+ (square p) (square q))
                  (+ (square q) (* 2 p q))
                  (/ count 2)))
        (else (fib-itr (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p q (- count 1)))))