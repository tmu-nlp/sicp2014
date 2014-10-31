;sicp_1_4.scm
(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b))
	
(a-plus-abs-b 10 -10)