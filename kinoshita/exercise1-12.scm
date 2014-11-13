(define (pl n k)
    (cond
	((= n 0) 1)
	((= k 0) 1)
	((= n k) 1)
	(else (+ (pl (- n 1) (- k 1)) (pl (- n 1) k)))))

(print (pl 0 0))
(print (pl 1 0) (pl 1 1))
(print (pl 2 0) (pl 2 1) (pl 2 2))
(print (pl 3 0) (pl 3 1) (pl 3 2) (pl 3 3))
