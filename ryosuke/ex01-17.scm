(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? a)
  (= (modulo a 2) 0))

(define (my* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (my* a (halve b))))
        (else (+ a (my* a (- b 1))))))


