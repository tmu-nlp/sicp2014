; 絶対値を足すので終わりの二行は同じ値になる。

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print (a-plus-abs-b 4 2))
(print (a-plus-abs-b 4 -2)) 
