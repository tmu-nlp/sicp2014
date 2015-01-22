;ex2_18.scm
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;引数としてリストをとり, 同じ要素の逆順のリストを返す手続き 
(define (reverse items)
  (define (reverse-iter x y)
    (if (null? x)
        y
        (reverse-iter (cdr x) (cons (car x) y))))
  (reverse-iter items nil))

(reverse (list 1 4 9 16 25))