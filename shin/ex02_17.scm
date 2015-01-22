#!/usr/bin/gosh
;; -*- coding:utf-8 -*-

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

#?=(last-pair (list 23 72 149 34))


