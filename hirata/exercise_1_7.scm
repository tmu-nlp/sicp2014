#!/usr/local/bin/gosh

(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) (* 2 x))) 0.001))	;wikipediaとかに書いてある、漸化式を利用する。
