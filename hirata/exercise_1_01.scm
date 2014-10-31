#!/opt/local/bin/gosh

(print "10 : " 10)
(print "(+ 5 3 4) : "(+ 5 3 4))
(print "(- 9 1) : "(- 9 1))
(print "(+ (* 2 4)(- 4 6)) : "(+ (* 2 4)(- 4 6))) 
(define a 3)
(define b (+ a 1))
(print "(+ a b (* a b)) : "(+ a b (* a b)))
(print "(= a b) : "(= a b))
(print "(if (and (> b a)(< b (* a b)))
	 b
	 a) : "(if (and (> b a)(< b (* a b)))
	 b
	 a))
(print "(cond ((= a 4) 6)		;cond:条件分岐
	     ((= b 4)(+ 6 7 a))
	     (else 25)) : "(cond ((= a 4) 6)		;cond:条件分岐
	     ((= b 4)(+ 6 7 a))
	     (else 25)))
(print "(+ 2 (if (> b a) b a)) : "(+ 2 (if (> b a) b a)))
(print "(* (cond ((> a b) a)
	        ((< a b) b)
	        (else -1))
	  (+ a 1)) : "(* (cond ((> a b) a)
	        ((< a b) b)
	        (else -1))
	  (+ a 1)))
