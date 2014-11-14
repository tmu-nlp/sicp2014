;recursive
(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(print (f 2 ))
(print (f 3 ))
(print (f 4 ))

;iteration
(define (f n)
  (define (func a b c count)
    (if (>= count n) a 
      (func (+ a (* 2 b) (* 3 c)) a b (+ count 1))))
  (func 2 1 0 0))

(print (f 2 ))
(print (f 3 ))
(print (f 4 ))

