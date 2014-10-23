;;if b is negative number, minus is used as operators.
;;otherwise plus is used.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print (a-plus-abs-b 4 5))
(print (a-plus-abs-b 4 -5))
