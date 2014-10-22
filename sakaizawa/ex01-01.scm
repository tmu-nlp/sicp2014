#!/usr/bin/gosh
;;-*- coding:utf-8 -*-
;;実行方法 gosh ex01-01.scm

(print "10")
(print 10)
(print )

(print "(+ 5 3 4)")
(print (+ 5 3 4))
(print )

(print "(- 9 1)")
(print (- 9 1))
(print )

(print "(/ 6 2)")
(print (/ 6 2))
(print )

(print "(+ (* 2 4) (- 4 6))")
(print (+ (* 2 4) (- 4 6)))
(print )

(print "(define a 3)")
(print (define a 3))
(print )

(print "(define b (+ a 1))")
(print (define b (+ a 1)))
(print )

(print "(+ a b (* a b))")
(print (+ a b (* a b)))
(print )

(print "(= a b)")
(print (= a b))
(print )

(print "(if (and (> b a) (< b (* a b)))
    b   
    a)") 
(print (if (and (> b a) (< b (* a b)))
    b   
    a)) 
(print )

(print "(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a)) 
      (else 25))")
(print (cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a)) 
      (else 25)))
(print )

(print "(+ 2 (if (> b a) b a))")
(print (+ 2 (if (> b a) b a)))
(print )

(print "(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))")
(print (* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)))
