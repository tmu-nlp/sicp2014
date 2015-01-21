(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(print (fold-right / 1 (list 1 2 3)))

(print (fold-left / 1 (list 1 2 3)))

(print (fold-right list (list) (list 1 2 3)))

(print (fold-left list (list) (list 1 2 3)))

(print (fold-right + 0 (list 1 2 3)))

(print (fold-left + 0 (list 1 2 3)))

(print (fold-right * 1 (list 1 2 3)))

(print (fold-left * 1 (list 1 2 3)))

(print (fold-right - 0 (list 1 2 3)))

(print (fold-left - 0 (list 1 2 3)))

;fold-rightの場合、listのそれぞれの要素に対してopの作用が再帰を繰り返すたびに影響してしまう
;(例えば (print (fold-right - 0 (list 1 2 3))) の場合、要素3には-が二回、要素2には-が一回作用するため1-2-(-3)-(-(-0))=2となっていた。)
;ため、opはその作用を何度しても作用が変わらない作用(+や*)でないとfold-rightとfold-leftの結果が変わる。