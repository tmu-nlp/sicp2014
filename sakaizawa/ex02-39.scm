#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-39.scm

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(print "use fold-right")
(print "(reverse (list 1 2 3))")
(print (reverse (list 1 2 3)))
(print )

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

(print "use fold-left")
(print "(reverse (list 1 2 3))")
(print (reverse (list 1 2 3)))


