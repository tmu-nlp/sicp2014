(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
  (else else-clause)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (average x y)
  (/ (+ x y) 2.0))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(print (sqrt-iter 1 3))

;ifは選択した命令のみを実行するが，
;condの場合は全ての命令を実行するため
;sqrt-iterを無限に繰り返してしまう．
