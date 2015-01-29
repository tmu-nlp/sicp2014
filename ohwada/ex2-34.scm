(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; horner-eval
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                      (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))


(print (horner-eval 2 (list 1 3 0 5 0 1))) ; 1 + 3*2 + 5*8 + 32 = 79
