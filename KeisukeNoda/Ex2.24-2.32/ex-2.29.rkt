#lang racket

;; 2進モービルを二つの枝からできている
;; 合成データで表現
(define (make-mobile left right)
  (list left right))

;; 一つの枝はlength(数でなければならない)と,
;; 数か別のモービルである structure で構成する
(define (make-branch length structure)
  (list length structure))


;; a.
;; モービルの枝を返す手続き
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;; 枝の部品を返す手続き
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;; b.
;; aで定義した選択肢を使って, モービルの全重量を
;; 返す手続きtotal-weightを定義する
(define (mobile? item)
  (pair? item))

(define (total-weight structure)
  (if (not (mobile? structure)) structure
  (+ (total-weight (branch-structure (left-branch structure)))
     (total-weight (branch-structure (right-branch structure))))))

(define br1 (make-branch 14 5))
(define br2 (make-branch 5 2))
(define br3 (make-branch 10 (make-mobile br1 br2)))
(define br4 (make-branch 10 (make-mobile br1 br1)))

(define m (make-mobile br1 br3))

(total-weight m) 
;; -> 5 + 2 + 5 -> 12

;; c.
;; 二進モービルが釣り合っているか釣り合ってるかどうかのテスト
;; 回転力:長さ×重さ

;; 最上段で左右の枝が釣り合っているか調べる
;; 釣り合っていたら、再帰的に双方の枝が釣り合っているか調べる
;; 釣り合っていない枝が見つかったら終了

(define (mobile-balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (if (mobile? mobile) 
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (= (torque left) (torque right)) 
             (mobile-balanced? (branch-structure left))
             (mobile-balanced? (branch-structure right))))
      true))


(mobile-balanced? (make-mobile br1 br1)) ;-> true
(mobile-balanced? (make-mobile br4 br4)) ;-> true
(mobile-balanced? m)                     ;-> false

;; d.
;; make-mobile, make-branchの実装が変わった時の対応