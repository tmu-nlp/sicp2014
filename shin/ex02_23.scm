#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (for-each func items)
  (cond ((null? items) (newline))
        (else (func (car items))
              (for-each func (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
