#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-37.scm

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define v (list 1 2))
(print "v: " v)
(define w (list 3 4))
(print "w: " w)
(define m (list (list 1 2) (list 3 4)))
(print "m: " m)

;;内積
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(print "(dot-product v w)")
(print (dot-product v w))

;;matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))
(print "(matrix-*-vector m v)")
(print (matrix-*-vector m v))

;;転置
(define (transpose mat)
  (accumulate-n cons () mat))
(print "(transpose m)")
(print (transpose m))

;;matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
       (map (lambda (x) (matrix-*-vector cols x)) m)))
(print "(matrix-*-matrix m m)")
(print (matrix-*-matrix m m))


