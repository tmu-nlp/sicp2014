;recursive
(define (f n)
    (if (< n 3)
	n
	(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(print (f 2))
(print (f 7))

;iterative
(define (f n)
    (if (< n 3)
	n
	(f-iter 2 1 0 2 n)))
(define (f-iter a b c iter max-count)
    (if (= max-count iter)
	a
	(f-iter (+ a (* 2 b) (* 3 c)) a b (+ 1 iter) max-count)))
(print (f 2))
(print (f 7))
