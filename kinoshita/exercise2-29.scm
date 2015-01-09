(define (make-mobile left right)
;    (list left right))
    (cons left right))
(define (make-branch length structure)
;    (list length structure))
    (cons length structure))

;a
(define (left-branch mobile)
    (car mobile))
(define (right-branch mobile)
;    (car (cdr mobile)))
    (cdr mobile))
(define (branch-length branch)
    (car branch))
(define (branch-structure branch)
;    (car (cdr branch)))
    (cdr branch))

;b
(define (branch-weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

;c
(define (balanced? mobile)
    (define (torque branch)
        (* (branch-length branch)
           (branch-weight branch)))
    (define (branch-balanced? branch)
        (if (pair? (branch-structure branch))
            (balanced? (branch-structure branch))
            #t))
    (and (= (torque (left-branch mobile))
            (torque (right-branch mobile)))
         (branch-balanced? (left-branch mobile))
         (branch-balanced? (right-branch mobile))))

(define r (make-branch 4 5))
(define l1 (make-branch 3 8))
(define l2 (make-branch 2 3))
(define l (make-branch 2 (make-mobile l1 l2)))
(define mobile (make-mobile l r))

(print (total-weight mobile)) 

(print (balanced? (make-mobile (make-branch 2 3) (make-branch 2 3))))
(print (balanced? mobile))

