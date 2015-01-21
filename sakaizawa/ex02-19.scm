#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-19.scm

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (define (no-more? coins)
    (null? coins))
  (define (first-denomination coins)
    (car coins))
  (define (except-first-denomination coins)
    (cdr coins))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(print "(cc 100 us-coins)")
(print (cc 100 us-coins))

