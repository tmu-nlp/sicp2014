(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (square a)
    (* a a))

  (find-divisor n 2))

(print (smallest-divisor 199))
(print (smallest-divisor 1999))
(print (smallest-divisor 19999))
; because 19999 is not prime number
