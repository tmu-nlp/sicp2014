; gosh ex01-29.scm

; computing integral by Simpson's Rule
(define (simpsons f a b n)
  (define (even? x)
    (= 0 (remainder x 2)))

  (define h (/ (- b a) n))

  (define (yk k a h)
    (f (+ a (* k h))))
  
  (define (iter i temp)
    (cond ((> i n) temp)
          ((= i 0) (iter (+ 1 i) (+ temp (yk 0 a h))))
          ((= i n) (iter (+ 1 i) (+ temp (yk i a h))))
          ((even? i) (iter (+ 1 i) (+ temp (* 2 (yk i a h)))))
          (else (iter (+ 1 i) (+ temp (* 4 (yk i a h)))))))
  
  (* (/ h 3) (iter 0 0)))

; test

(define (cube x)
  (* x x x))

(print "integral between a and b")
(print "(simpsons f a b n)")

(print "(simpsons cube 0 1 100)")
(print (simpsons cube 0 1 100))

(print "(simpsons cube 0 1 1000)")
(print (simpsons cube 0 1 1000))

