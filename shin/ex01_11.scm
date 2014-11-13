#!/usr/bin/gosh

;再帰的
(define (f n)
    (if (< n 3) n
        (+ (f (- n 1)) 
        	(* 2 (f (- n 2))) 
        	(* 3 (f (- n 3))))))

;反復的
(define (f2 n)
    (define (iter new old old2 count)
        (if (>= count n) new
            (iter (+ new (* 2 old) (* 3 old2)) new old (+ 1 count))))
    (iter 4 2 1 3))

#?=(f 10)
#?=(f2 10)