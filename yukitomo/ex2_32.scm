;ex2_32.scm

(define (subsets a)
  (if (null? a)
      (list nil)
      (let ((rest (subsets (cdr a))))
        (append rest (map (lambda (x) (cons (car a) x)) rest)))))

(display (subsets (list 1 2)))

