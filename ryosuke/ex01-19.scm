(define (even? n)
  (= (modulo n 2) 0))

(define (fib n)
  (fib_iter 1 0 0 1 n))

(define (fib_iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib_iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else
          (fib_iter (+ (* b q) (* a q) (* a p))
                    (+ (* q a) (* p b))
                    p
                    q
                    (- count 1)))))
