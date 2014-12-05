; 逐次平方を用いた反復的べき乗計算のプロセス

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))


(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))


(print (fast-expt 2 32))
(print (fast-expt 3 8))
