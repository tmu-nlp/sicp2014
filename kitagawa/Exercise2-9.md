####Exercise2-9
---
intervalの和と差それぞれについて
結果の幅が元の幅から計算できることを示す

	x, y : interval

	width x = (/ (- (upper-bound x) (lower-bound x)) 2)

	wx = width(x), wy = width(y)

	wadd = (width (add-interval x y))
      = (width (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y))))
      = (/ (- (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y))) 2)
      = (/ (+ (- (upper-bound x) (lower-bound x)) (- (upper-bound y) (lower-bound y))) 2)
      = (/ (+ (* (width x) 2) (* (width y) 2)) 2)
      = (+ (width x) (width y))


	wsub = (width (sub-interval x y))
      = (width (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))
      = (/ (- (- (upper-bound x) (lower-bound y)) (- (lower-bound x) (upper-bound y))) 2)
      = (/ (+ (- (upper-bound x) (lower-bound x)) (- (- (upper-bound y) (lower-bound y)))) 2)
      = (/ (- (* (width x) 2) (* (width y) 2)) 2)
      = (- (width x) (width y))

 となるのでaddとsubについてはwidthから一意に求まることが示された

 積と商についての例は

 例えばA(-1 . 3) B(-2 . 4)の2つの区間を考えると
　幅はそれぞれ2, 3であるが
掛け算の区間(-6, 12)の幅は9となり幅だけの関数では表わせなさそう
割り算も同様で区間は(-1/2, 3/4)でありこの幅は5/8でこれは幅だけでは表せない

たぶん掛け算も分数が出てきて幅だけでは表せない具体例を探せば良いかもしれない
