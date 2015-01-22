#lang racket

; extracting 7 from:

(car (cdaddr '(1 3 (5 7) 9)))
;; cdr => cdr => car => cdr => car 

(caar '((7)))
;; car => car

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))
;; cdr => car => cdr => car  => cdr => car => cdr => car => cdr => car => cdr => car

