#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle c)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0) #?=(+ c 1)))))

#?=(sine 12.15 0)
