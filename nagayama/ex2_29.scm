#lang scheme

; binary mobile
; リストで構成された木
; モビールには左右の枝の情報
; 枝には長さと先にぶら下がっているものの情報
 

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; [a] left-branch, right-branch を定義
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; [b] total-weight を定義
(define (total-weight mobile)
  (if (list? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

; [c] balanced? を定義
(define (balanced? mobile)
  (if (list? mobile)
    (if (= (torque (left-branch mobile))
           (torque (right-branch mobile)))
        (and (balanced? (branch-structure (left-branch mobile)))
             (balanced? (branch-structure (right-branch mobile))))
        #f)
    #t))

(define (torque branch)
  (if (list? branch)
      (* (total-weight (branch-structure branch))
         (branch-length branch))
      #f))
    
; [d] list -> cons に変更したら?
(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))

; right-branch
; branch-structure
; に変更が必要



; 以下、実行テスト

;   -- a -
;  4    - b -
;      2   - c -
;         1     1
; 

(define cl (make-branch 1 1))
(define cr (make-branch 1 1))
(define c (make-mobile cl cr))
(define bl (make-branch 1 2))
(define br (make-branch 1 c))
(define b (make-mobile bl br))
(define al (make-branch 1 4))
(define ar (make-branch 1 b))
(define a (make-mobile al ar))

(total-weight a)

(balanced? c)
(balanced? b)
(balanced? a)

