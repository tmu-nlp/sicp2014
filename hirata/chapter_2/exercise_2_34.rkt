(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ (* x higher-terms) this-coeff))
              0 
              coefficient-sequence))

(display (horner-eval 2 (list 1 3 0 5 0 1)))