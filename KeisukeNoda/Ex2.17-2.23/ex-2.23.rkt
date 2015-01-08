#lang racket

;; for-eachの実装
(define (for-each f items)
  (if (null? items)
      true
      (and (f (car items))
           (for-each f (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;; ->57
;; ->321
;; ->88#t