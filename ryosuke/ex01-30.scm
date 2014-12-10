; gosh ex01-30.scm

; original (linear recursion)
(define (orig-sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; iteratively
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))


; test
(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(print "(sum cube 1 inc 10)")
(print "origin")
(print (orig-sum cube 1 inc 10))
(print "iteratively")
(print (sum cube 1 inc 10))


