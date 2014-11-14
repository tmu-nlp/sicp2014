;ex1_25.scm

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
