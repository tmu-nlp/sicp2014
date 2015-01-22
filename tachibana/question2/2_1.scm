#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (make-rat x y) (if (< (/ x y) 0) (- 0 (/ x y)) (/ x y) ) )