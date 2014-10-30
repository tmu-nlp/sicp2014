; if number of digits increases, remainder function is too slow to run.
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
