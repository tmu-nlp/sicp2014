#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define mobile (make-mobile 2 4))
(print "(define mobile (make-mobile 2 4))")

;(a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;(b)
(define (total-weight m)
  (if (not (pair? m))
      m
      (let ((l-b (left-branch m))
            (r-b (right-branch m)))
        (let ((l-s (branch-structure l-b))
              (r-s (branch-structure r-b)))
          (+ (total-weight l-s)
             (total-weight r-s))))))

;(c)
(define (balanced m)
  (if (not (pair? m))
      #t
      (let ((left-b (left-branch m))
            (right-b (right-branch m)))
        (let ((left-l (branch-length left-b))
              (right-l (branch-length right-b))
              (left-s (branch-structure left-b))
              (right-s (branch-structure right-b)))
          (and (= (* left-l (total-weight left-s))
                  (* right-l (total-weight right-s)))
               (balanced left-s)
               (balanced right-s))))))

;(d)
;変更には対応できる抽象化を行っているので、
;right-branchとbranch-structureで使っているcadrをcdrに変更するだけでオーケー


----------
;; -*- Scheme -*-
;; 
;; Code for Exercise 2.29
;; $Id: ex-2.29.scm,v 1.1 2004/12/12 23:25:49 nobsun Exp $
;;----------------------------------------------------------

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;----------------------------------------------------------

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left)
       (branch-weight right))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))

(define (balanced? mobile)
  (if (balanced-mobile? mobile) #t #f))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (let ((left-weight (balanced-branch? left)))
      (and left-weight
           (let ((right-weight (balanced-branch? right)))
             (and right-weight
                  (= (* (branch-length left) left-weight)
                     (* (branch-length right) right-weight))
                  (+ left-weight right-weight)))))))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (balanced-mobile? structure)
        structure)))


;                    |
;       +------------+----+
;       |                 |
;   +---+-+        +------+--+
;   |     |        |         |
;   1     3     +--+-+  +----+--+
;               |    |  |       |
;               1    2  3     +-+-----+
;                             |       |
;                             5       1
; 

(define a-mobile
  (make-mobile
   (make-branch 12 (make-mobile
                    (make-branch 3 1)
                    (make-branch 1 3)))
   (make-branch  4 (make-mobile
                    (make-branch 6 (make-mobile
                                    (make-branch 2 1)
                                    (make-branch 1 2)))
                    (make-branch 2 (make-mobile
                                    (make-branch 4 3)
                                    (make-branch 2 (make-mobile 
                                                    (make-branch 1 5)
                                                    (make-branch 5 1)))))))))

;(total-weight a-mobile)
;(balanced? a-mobile)

;(define (make-mobile left right) (cons left right))
;(define (make-branch length structure) (cons length structure))
;(define (right-branch mobile) (cdr mobile))
;(define (branch-structure branch) (cdr branch))