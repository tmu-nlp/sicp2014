; bに絶対値をとった値とaの和が表示される
(define (a-plus-abs-b a b)
        ((if (> b 0) + -) a b))

