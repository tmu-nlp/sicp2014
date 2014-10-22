#!/usr/bin/gosh

(print "a. " 10)

(print "b. " (+ 5 3 4))

(print "c. " (- 9 1))

(print "d. " (/ 6 2))

(print "e. " (+ (* 2 4) (- 4 6)))

(print "f. " (define a 3))

(print "g. " (define b (+ a 1)))

(print "h. " (+ a b (* a b)))

(print "i. " (= a b))

(print "j. " (if (and (> b a) (< b (* a b)))
    b   
    a)) 

(print "k. " (cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a)) 
      (else 25)))

(print "l. " (+ 2 (if (> b a) b a)) )

(print "m. " (* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)))
