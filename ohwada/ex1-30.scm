; sum の反復的な定義
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(define (cube x) (* x x x))


(print (sum-cubes 1 3)) ; 1+8+27=36
