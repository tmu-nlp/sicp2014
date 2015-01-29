(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;b
(define (total-weight mobile)
  (if (list? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile)
)

;c
(define (balanced? mobile)
  (if (list? mobile)
  	(if (= (torque (left-branch mobile)) (torque (right-branch mobile)))
  	  (cond
  	  	((and (list? (branch-structure (right-branch mobile))) (list? (branch-structure (left-branch mobile))))
  	  		(and (balanced? (branch-structure (right-branch mobile))) (balanced? (branch-structure (left-branch mobile)))))
  	  	((list? (branch-structure (right-branch mobile))) (balanced? (branch-structure (right-branch mobile))))
  	  	((list? (branch-structure (left-branch mobile))) (balanced? (branch-structure (left-branch mobile))))
  	  	(else #t))
      #f)
    #f))

(define (torque branch) (* (branch-length branch) (total-weight (branch-structure branch))))

;d
;cadrをcdrにすればよい
