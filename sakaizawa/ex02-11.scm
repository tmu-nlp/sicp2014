#!/usr/bin/gosh
;; -*- coding:utf-8 -*-
;;実行方法　gosh ex02-11.scm

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (new-mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((< xu 0)
           (cond ((< yu 0) (make-interval (* xu yu) (* xl yl)))
                 ((< yl 0) (make-interval (* xl yu) (* xl yl)))
                 (else (make-interval (* xl yu) (* xu yl)))))
          ((< xl 0)
           (cond ((< yu 0) (make-interval (* xu yl) (* xl yl)))
                 ((< yl 0) (make-interval (min (* xl yu) (* xu yl))
                                        (max (* xl yl) (* xu yu))))
                 (else (make-interval (* xl yu) (* xu yu)))))
          (else
           (cond ((< yu 0) (make-interval (* xu yl) (* xl yu)))
                 ((< yl 0) (make-interval (* xu yl) (* xu yu)))
                 (else (make-interval (* xl yl) (* xu yu))))))))

(define r1 (make-interval 6 8))
(print "(define r1 (make-interval 6 8))")
(define r2 (make-interval 7 9))
(print "(define r2 (make-interval 7 9))")
(print )
(print "(new-mul-interval r1 r2)")
(print (new-mul-interval r1 r2))
(print )


