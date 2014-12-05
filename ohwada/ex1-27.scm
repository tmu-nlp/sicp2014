; Carmechael数がFermatテストをだますことを検証するための手続き

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x) (* x x))


(define (fermat-test n a)
    (= (expmod a n n) a))



(define (carmechael-test n a)
  (cond ((= a n) #t)
        ((fermat-test n a) (carmechael-test n (+ a 1)))
        (else #f)))

; テスト

(print 561 (carmechael-test 561 1))
(print 1105 (carmechael-test 1105 1))
(print 1729 (carmechael-test 1729 1))
(print 2465 (carmechael-test 2465 1))
(print 2821 (carmechael-test 2821 1))
