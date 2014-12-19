#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-25.scm

(define x (list 1 3 (list 5 7) 9))
(print "(define x (list 1 3 (list 5 7) 9))")
(define y (list (list 7)))
(print "(define y (list (list 7)))")
(define z (list 1
                (list 2
                      (list 3
                            (list 4
                                  (list 5
                                        (list 6 7)))))))
(print "(define z (list 1
                (list 2
                      (list 3
                            (list 4
                                  (list 5
                                        (list 6 7)))))))")

(print "(car (cdr (car (cdr (cdr x)))))")
(print (car (cdr (car (cdr (cdr x))))))
(print "(car (car y))")
(print (car (car y)))
(print "(cadr (cadr (cadr (cadr (cadr (cadr z))))))")
(print (cadr (cadr (cadr (cadr (cadr (cadr z)))))))


