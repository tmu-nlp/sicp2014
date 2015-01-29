;ex2_27.scm

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

;deep-reverse
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))
      items))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)
