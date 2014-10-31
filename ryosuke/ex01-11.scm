; recursive
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

; iterative
(define (fi n)
  (define (fi-iter a b c count)
    (cond ((< count 3) a)
          (else (fi-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (cond ((< n 3) n)
        (else (fi-iter 2 1 0 n))))


