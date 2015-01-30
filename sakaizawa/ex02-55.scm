#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-55.scm

#?=(car ''abracadabra)

;''abracadabra ->  (quote (quote abracadabra))
;(car (quote (quote abracadabra))) -> (car '(quote abracadabra))
;(car ''abracadabra) -> quote 

