####フィボナッチ数列の一般項を求める

---

	Fn = Fn-1 + Fn-2 , F1 = 0 F2 =1 の一般項Fnを求める
	x^2=x+1の解をα=(1+√2)/√5,β=(1−√2)/√5とおくと，α+β=1,αβ=−1より
	Fn=(α+β)Fn−1−αβFn−2
	αとβを入れ替えて2式を作る

	Fn−αFn−1=β(Fn−1−αFn−2)
	Fn−βFn−1=α(Fn−1−βFn−2)

	よって，Fn−αFn−1, Fn-βFn-1は公比β,αの等比数列となる

	Fn−αFn−1=βn−2(F2−αF1)=β^n−1 ... ①
	Fn−βFn−1=αn−2(F2−βF1)=α^n−1 ... ②

	① - ② を実行しFnを消去してFn-1について解く
	添字n-1をnに変更し
	Fn=(β^n -α^n)/√5	
	を得る


####Fnが最も近い整数をαn/√5で近似できることを示す
---
Fnの第2項目がは(-1/2,1/2)で抑えられることを示せば良いと説明したが
これは間違えていることが分かったのでこのページはまた更新します