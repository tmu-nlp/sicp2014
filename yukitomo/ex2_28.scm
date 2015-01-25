;ex2_28.scm

(define (fringe l)
  (if (pair? l)
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (cons (car l) (fringe (cdr l))))
      l))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))