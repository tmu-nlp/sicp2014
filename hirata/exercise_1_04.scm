#!/usr/local/bin/gosh

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print (a-plus-abs-b 1 2))
(print (a-plus-abs-b 1 -2))	;bの絶対値をとる。ifでbが0以上か以下かを判断し、0以上だったら+の演算子・0以下だったら-の演算子をとる。
