; if number of digits increases, remainder function is too slow to run.
; above answer is failed, following answer is correct answer
; if number of digits increases, fast-expt function is too slow to ran.
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
