;ex2_29.scm

;二進モービル　右の枝左の枝構成
(define (make-mobile left right)
  (list left right))

;枝
(define (make-branch length structure)
  (list length structure))


;(a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))


;(b)
(define (total-weight m)
  (if (not (pair? m))
      m
      (let ((l-b (left-branch m))
            (r-b (right-branch m)))
        (let ((l-s (branch-structure l-b))
              (r-s (branch-structure r-b)))
          (+ (total-weight l-s)
             (total-weight r-s))))))

;(c)
(define (balanced m)
  (if (not (pair? m))
      #t
      (let ((left-b (left-branch m))
            (right-b (right-branch m)))
        (let ((left-l (branch-length left-b))
              (right-l (branch-length right-b))
              (left-s (branch-structure left-b))
              (right-s (branch-structure right-b)))
          (and (= (* left-l (total-weight left-s))
                  (* right-l (total-weight right-s)))
               (balanced left-s)
               (balanced right-s))))))

;(d)
;変更には対応できる抽象化を行っているので、right-branchとbranch-structureで使っているcadrをcdrに変更するだけでオーケー

