#lang scheme

; for-each 手続きの実装

(define (for-each f s)
  (cond ((null? s) (newline))
        (else (f (car s))
              (for-each f (cdr s)))))

; run
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
