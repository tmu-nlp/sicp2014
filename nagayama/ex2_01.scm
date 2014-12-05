#lang racket

; 負数も扱える make-rat

(load "Ratio.scm")

(define (make-rat n d)
  (if (= d 0) #f
    (let ((g (gcd n d)))
      (if (> d 0) 
          (cons (/ n g) (/ d g))
          (cons (* -1 (/ n g))
                (* -1 (/ d g)))))))
