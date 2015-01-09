#lang racket

;; 集合の全ての部分集合の集合を表現するべき集合

;; べき集合Xは集合Xの全ての部分集合の集合
;; => Xのある要素を抜き出した集合の部分集合と、各部分集合にXを加えた和集合
;; => subsets内のrestが先頭要素を取り除いたもの
;; => そこに取り除いた先頭要素を加えるー
;; (subset (cdr (1 2 3)))
;; => (subset (2 3)) だから 1を加えるような処理
;; => (append (map <1を加えるような手続き> rest))
;; => 空欄部分は(append (map <(lambda (x) (cons (car s) x))> rest))


(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

