(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multi-iter n a b)
  (cond ((= b 0) n)
        ((even? b) (multi-iter n (double a) (halve b)))
        (else (multi-iter (+ n a) a (- b 1)))))

(define (multi a b)
  (multi-iter 0 a b))

(multi 7 8)
;; => 56