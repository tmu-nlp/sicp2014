(define (double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (even? a)
  (= (modulo a 2) 0))

(define (my* a b)
  (let ((n 0))
    ; n+a*b
    (define (iter n a b)
      (cond ((= b 0) n)
            ((even? b) (iter n (double a) (halve b)))
            (else (iter (+ n a) a (- b 1)))))
    (iter n a b)))

