#!/usr/local/bin/gosh

;再帰的プロセス
(define (f n)
    (if (< n 3)
	    n
		(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(print (f 3))

;反復的プロセス
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(print (fib 4))
