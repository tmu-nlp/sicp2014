#!/usr/bin/env gosh
;; coding: utf-8
;; 
;; Author: Peinan ZHANG
;; Created at: 2014-11-14

(define (pascal x y)
  (cond ((= y 1) 1)
    ((= x y) 1)
    (else (+ (pascal (- x 1) (- y 1))
      (pascal (- x 1) y)))))

#?=(pascal 3 2)
#?=(pascal 5 3)
