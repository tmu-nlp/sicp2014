#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (split func1 func2)
  (lambda (painter n)
    (let ((smaller (if (= n 0) painter ((split func1 func2) painter (- n 1)))))
      (func1 painter (func2 smaller smaller)))))

(define right-split (split beside below))
(define up-split (split below beside))

;DrRacket で上記を定義した後
;(paint (up-split wave 4))
