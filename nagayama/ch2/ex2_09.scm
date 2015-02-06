#lang scheme

; 区間の幅における加減乗除について

(define (wid x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

#|

x = [a, b]
y = [c, d]
x+y -> [a+c, b+d]
x-y -> [a-c, b-d]


wid(x) -> (b-a)/2
wid(y) -> (d-c)/2
wid(x+y)
 -> ((b+d) - (a+c))/2 = (b-a)/2 + (d-c)/2 -> wid(x)+wid(y)
wid(x-y)
 -> ((b-d) - (a-c))/2 = (b-a)/2 - (d-c)/2 -> wid(x)-wid(y)

よって, 加算減算については確認できた
また, 乗算除算については
     
wid(x)*wid(y) -> ((b-a)*(d-c))/4  -> (abcdのみの有理式)/4
wid(x)/wid(y) -> (b-a)/(d-c)      -> (abcdのみの有理式)
であるのに対して
wid(x*y) -> (abcdのみの有理式)/2
wid(x/y) -> (abcdのみの有理式)/2
であることから一般に成り立つとは言えないことがわかる

|#
