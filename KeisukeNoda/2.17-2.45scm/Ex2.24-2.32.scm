;;;; Exercise 2.24 ;;;;

;; これは絵を書く
(list 1 (list 2 (list 3 4)))
;; gosh> (1 (2 (3 4)))


;;;; Exercise 2.25 ;;;;


(1 3 (5 7) 9)
(define x (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr x)))))
;; gosh> 7
;; cdr => cdr => car => cdr => car 

((7))
(define x (list (list 7)))
(car (car x))
;; gosh> 7
;; car => car 
(caar x)
;; gosh> 7

(1 (2 (3 (4 (5 (6 7))))))
(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))
;; gosh> 7
;; cdr => car
;; => cdr => car 
;; => cdr => car
;; => cdr => car
;; => cdr => car
;; => cdr => car

;;;; Exercise 2.26 ;;;;


(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;; gosh> (1 2 3 4 5 6)

(cons x y)
;; gosh> ((1 2 3) 4 5 6)

(list x y)
;; gosh> ((1 2 3) (4 5 6))

;;;; Exercise 2.27 ;;;;


(define x (list (list 1 2) (list 3 4)))

;; reverseはgauche組み込みであるけど、念のために
(define (reverse-new items)
  (define (reverse-iter rev-items items)
    (if (null? items)
	rev-items
	(reverse-iter (cons (car items) rev-items) (cdr items))))
  (reverse-iter () items))

(reverse-new (list 1 2 3 4))

;; 最初に思いついた回答
;; 対なら(cdr, car)でくっつけて再帰的にたどっていく
(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))
      items))

(deep-reverse x)

;; mapを使う
;; 葉まで分解し、再構築してreverseしながらくっつけていく
(define (deep-reverse items)
   (if (not (pair? items))
      items
      (reverse (map deep-reverse items))))

x
;; gosh> ((1 2) (3 4))

(reverse x)
;; gosh> ((3 4) (1 2))

(deep-reverse x)
;; gosh> ((4 3) (2 1))

;;;; Exercise 2.28 ;;;;


(define x (list (list 1 2) (list 3 4)))

;; count-leavesを流用する
(define (fringe items)
  (cond [(null? items) items]
	[(not (pair? items)) (list items)]
	[else (append (fringe (car items))
		      (fringe (cdr items)))]))

(fringe x)
;; gosh> (1 2 3 4)


(fringe (list (list 1 2) (list (list 3 4) 5 (list 6 7)) (list 8 9)))
;; gosh> (1 2 3 4 5 6 7 8 9)

;;;; Exercise 2.29 ;;;;


;; 2進モービルを二つの枝からできている
;; 合成データで表現
(define (make-mobile left right)
  (list left right))

;; 一つの枝はlength(数でなければならない)と,
;; 数か別のモービルであるstructureで
;; 構成する
(define (make-branch length structure)
  (list length structure))

;; a.
;; モービルの枝を返す手続き
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

;; 枝の部品を返す手続き
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; aの確認
(define mobile1 (make-mobile (make-branch 1 2)
			     (make-branch 3 4)))

(define mobile2 (make-mobile (make-branch 6 2)
			     (make-branch 1 (make-mobile (make-branch 2 4)
							 (make-branch 1 8)))))
(print mobile1)
;; gosh> ((1 2) (3 4))

(left-branch mobile1)
;; gosh> (1 2)

(right-branch mobile1)
;; gosh> (3 4)

(branch-length (left-branch mobile1))
;; gosh> 1

(branch-structure (left-branch mobile1))
;; gosh> 2


;; b.
;; aで定義した選択肢を使って, モービルの全重量を
;; 返す手続きtotal-weightを定義する

;; mobileを与えて全重量を求めるtotal-weight
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(display mobile1)
;; gosh> ((1 2) (3 4))

(total-weight mobile1)
;; gosh> 6
;; 2 + 4 = 6

(display mobile2)
;; gosh> ((6 2) (1 ((2 4) (1 8))))

(total-weight mobile2)
;; gosh> 14
;; 2 + 4 + 8 = 14

;; c.
;; 二進モービルが釣り合っているか釣り合ってるかどうかのテスト
;; 回転力:長さ×重さ

;; 方針
;; 最上段で左右の枝が釣り合っているか調べる
;; 釣り合っていたら、再帰的に双方の枝が釣り合っているか調べる
;; 釣り合っていない枝が見つかったら終了

(define (balanced? mobile)
  (let ((left-moment (* (branch-length (left-branch mobile))
			(branch-weight (left-branch mobile))))
	(right-moment (* (branch-length (right-branch mobile))
			 (branch-weight (right-branch mobile)))))
    (if (= left-moment right-moment)
	(and (balanced-branch? (left-branch mobile))
	     (balanced-branch? (right-branch mobile)))
	#f)))

(define (balanced-branch? branch)
  (if (number? (branch-structure branch))
      #t
      (balanced? (branch-structure branch))))


;; 検証1.バランスがとれている
(define mobile3 (make-mobile (make-branch 1 2)
			     (make-branch 1 2)))
(balanced? mobile3)
;; gosh> #t

;; 検証2. ちょっと複雑だがバランスがとれている
(define mobile4 (make-mobile (make-branch 1
					  (make-mobile (make-branch 2 3)
						       (make-branch 3 2)))
			     (make-branch 1
					  (make-mobile (make-branch 3 2)
						       (make-branch 2 3)))))
(balanced? mobile4)

;; 検証3. ちょっと複雑だがバランスがとれていない
(define mobile5 (make-mobile (make-branch 3
					  (make-mobile (make-branch 6 3)
						       (make-branch 3 6)))
			     (make-branch 7 7)))
(balanced? mobile5)
;; gosh> #f

;; 検証4. 最上段はバランスがとれている。枝にぶら下がっているモービルでバランスがとれていない
(define mobile6 (make-mobile (make-branch 5
					  (make-mobile (make-branch 3 2)
						       (make-branch 3 4)))
			     (make-branch 5 6)))
(balanced? mobile6)



;; d.
;; make-mobile, make-branchの実装が変わった時の対応

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define mobile3 (make-mobile (make-branch 1 2)
			     (make-branch 1 2)))

(define mobile4 (make-mobile (make-branch 1
					  (make-mobile (make-branch 2 3)
						       (make-branch 3 2)))
			     (make-branch 1
					  (make-mobile (make-branch 3 2)
						       (make-branch 2 3)))))

;; 検証用
(display mobile3)
(left-branch mobile3)
(right-branch mobile3)
(branch-length (left-branch mobile3))
(branch-weight (left-branch mobile3))
(branch-length (right-branch mobile3))
(branch-weight (right-branch mobile3))
(total-weight mobile3)
(balanced? mobile3)

(display mobile4)
(left-branch mobile4)
(right-branch mobile4)
(branch-length (left-branch mobile4))
(branch-weight (left-branch mobile4))
(branch-length (right-branch mobile4))
(branch-weight (right-branch mobile4))
(total-weight mobile4)
(balanced? mobile4)


;; consとlistに対してcarとcdrを適用した時の結果を確認
(car (cons (cons 1 2) (cons 1 2)))
(car (list (list 1 2) (list 1 2)))

(cdr (cons (cons 1 2) (cons 1 2)))
(cdr (list (list 1 2) (list 1 2)))

;; P55図2.2とP56図2.4を見ればconsとlistの
;; データ構造の違いが理解できる

;; cadrを使っている手続きを修正する
;; right-branch, branch-structure
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

;; 検証用
(display mobile3)
(left-branch mobile3)
(right-branch mobile3)
(branch-length (left-branch mobile3))
(branch-weight (left-branch mobile3))
(branch-length (right-branch mobile3))
(branch-weight (right-branch mobile3))
(total-weight mobile3)
(balanced? mobile3)

(display mobile4)
(left-branch mobile4)
(right-branch mobile4)
(branch-length (left-branch mobile4))
(branch-weight (left-branch mobile4))
(branch-length (right-branch mobile4))
(branch-weight (right-branch mobile4))
(total-weight mobile4)
(balanced? mobile4)

;;;; Exercise 2.30 ;;;;


;; 再帰を利用したもの
(define (square-tree tree)
  (define nil '())
  (cond [(null? tree) nil]
	[(not (pair? tree)) (* tree tree)]
	[else (cons (square-tree (car tree))
		    (square-tree (cdr tree)))]))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; gosh> (1 (4 (9 16) 25) (36 49))



;; mapを利用したもの
(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; gosh> (1 (4 (9 16) 25) (36 49))

;;;; Exercise 2.31 ;;;;


(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; gosh> (1 (4 (9 16) 25) (36 49))


;;;; Exercise 2.32 ;;;;


;; 集合の全ての部分集合の集合を表現する
;; 冪集合の話

;; 冪集合Xは集合Xの全ての部分集合の集合
;; => Xのある要素を抜き出した集合の部分集合と、各部分集合にXを加えた和集合
;; => subsets内のrestが先頭要素を取り除いたもの
;; => そこに取り除いた先頭要素を加えればよい？？
;; (subset (cdr (1 2 3)))
;; => (subset (2 3)) だから 1を加えるような処理
;; => (append (map <1を加えるような手続き> rest))
;; => 空欄部分は(append (map <(lambda (x) (cons (car s) x))> rest))

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3)))
;; gosh> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; #<undef>


(use slib)
(require 'trace)
(trace subsets)
(print (subsets (list 1 2 3)))
;; gosh> CALL subsets (1 2 3)
;;   CALL subsets (2 3)
;;     CALL subsets (3)
;;       CALL subsets ()
;;       RETN subsets (())
;;     RETN subsets (() (3))
;;   RETN subsets (() (3) (2) (2 3))
;; RETN subsets (() (...) (...) (...) (1) (1 ...) (1 2) (1 2 3))


;; http://d.hatena.ne.jp/awacio/20100512/1273670118
