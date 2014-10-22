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

;if$B$OA*Br$7$?L?Na$N$_$r<B9T$9$k$,!$(B
;cond$B$N>l9g$OA4$F$NL?Na$r<B9T$9$k$?$a(B
;sqrt-iter$B$rL58B$K7+$jJV$7$F$7$^$&!%(B
