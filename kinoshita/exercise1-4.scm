(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))
(define a 4)
(define b -5)
(print
    (a-plus-abs-b a b)
)

(define a 4)
(define b 5)
(print (a-plus-abs-b a b))
;ifなどの返り値に+や-などの式をしていすることができる
