#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-41.scm

(define (square x) (* x x))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (unique-trio n)
  (flatmap (lambda (i)
                   (flatmap (lambda (j)
                                    (map (lambda (k) (list i j k))
                                         (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-of-trio trio)
  (+ (car trio) (cadr trio) (cadr (cdr trio))))

(define (equal-sum-of-trio n s)
  (filter (lambda (x) (= (sum-of-trio x) s)) (unique-trio n)))

(print "(unique-trio 6)")
(print (unique-trio 6))
(print "(equal-sum-of-trio 6 6)")
(print (equal-sum-of-trio 6 6))

