;expex1_27.scm

;Carmichael Number
;561, 1105, 1729, 2465, 2821, 6601

(define (square x) (* x x)) 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (is-prime? n)
  (define (itr a)
    (cond ((= a 0) #t)
          (else (if (= (expmod a n n) a)
                    (itr (- a 1)) #f))))
  (if (itr (- n 1))
      (display "I'ts prime.")
      (display "I'ts not prime.")))

(is-prime? 561)
(is-prime? 1105)